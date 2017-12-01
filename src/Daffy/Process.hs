module Daffy.Process
  ( Process
  , Output(..)
  , spawn
    -- * Some debugging functions
  , debugShowProcess
  , debugReadProcess
  ) where

import Control.Applicative
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, managed)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Streaming (Stream)
import Streaming.Prelude (Of)
import System.Exit (ExitCode)
import System.IO (Handle, hIsEOF)
import System.Process
  (CmdSpec(ShellCommand), CreateProcess(..), StdStream(CreatePipe, NoStream),
    createProcess, waitForProcess)

import qualified Control.Concurrent.Async as Async
import qualified Data.Text.IO as Text
import qualified Streaming (effect)
import qualified Streaming.Prelude as Streaming

data Output
  = Stdout Text
  | Stderr Text
  deriving (Eq, Show)

-- | A 'Process' is either still 'Running' or has 'Completed'.
data Process
  = Running (Seq Output) (Stream (Of Output) IO ExitCode)
  | Completed (Seq Output) ExitCode

-- | Spawn a managed 'Process'.
spawn :: String -> Managed (IO Process)
spawn path = do
  (Nothing, Just hout, Just herr, ph) <-
    liftIO (createProcess spec)

  outputSeq :: TVar (Seq Output) <-
    liftIO (newTVarIO mempty)

  outputChan :: TChan Output <-
    liftIO newBroadcastTChanIO

  doneSem :: TSem <-
    liftIO (atomically (newTSem (-1)))

  let append :: Output -> STM ()
      append x = do
        modifyTVar' outputSeq (|> x)
        writeTChan outputChan x

  let enqueueStdout :: IO ()
      enqueueStdout = do
        Streaming.mapM_ (atomically . append . Stdout) (fromHandle hout)
        atomically (signalTSem doneSem)

  let enqueueStderr :: IO ()
      enqueueStderr = do
        Streaming.mapM_ (atomically . append . Stderr) (fromHandle herr)
        atomically (signalTSem doneSem)

  -- Three concurrent actions: reading stdout, reading stderr, and waiting
  -- for the process to end.
  a1 :: Async () <-
    async enqueueStdout
  a2 :: Async () <-
    async enqueueStderr
  a3 :: Async ExitCode <-
    async (waitForProcess ph)

  let waitCode :: STM ExitCode
      waitCode = do
        Async.waitSTM a1
        Async.waitSTM a2
        Async.waitSTM a3

  let readCompletedProcess :: STM Process
      readCompletedProcess = do
        code <- waitCode
        output <- readTVar outputSeq
        pure (Completed output code)

  let readRunningProcess :: STM Process
      readRunningProcess = do
        output :: Seq Output <-
          readTVar outputSeq

        chan :: TChan Output <-
          dupTChan outputChan

        let stream :: Stream (Of Output) IO ExitCode
            stream = Streaming.effect (atomically (act1 <|> act2))
             where
              act1 :: STM (Stream (Of Output) IO ExitCode)
              act1 = do
                out <- readTChan chan
                pure (do
                  Streaming.yield out
                  stream)

              act2 :: STM (Stream (Of Output) IO ExitCode)
              act2 = do
                waitTSem doneSem
                code <- waitCode
                pure (pure code)

        pure (Running output stream)

  pure (atomically (readCompletedProcess <|> readRunningProcess))

 where
  spec :: CreateProcess
  spec = CreateProcess
    { cmdspec = ShellCommand path
    , cwd = Nothing
    , env = Nothing
    , std_in = NoStream
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = True
    , create_group = True
    , delegate_ctlc = False
    , detach_console = False
    , create_new_console = False
    , new_session = True
    , child_group = Nothing
    , child_user = Nothing
    , use_process_jobs = False
    }

debugShowProcess :: Process -> String
debugShowProcess = \case
  Running output _ -> "Running " ++ show output
  Completed output code -> "Completed " ++ show output ++ " " ++ show code

debugReadProcess :: Process -> IO ()
debugReadProcess = \case
  Running output stream -> do
    mapM_ print output
    code <- Streaming.mapM_ print (stream)
    print code
  Completed output code -> do
    mapM_ print output
    print code

async :: IO a -> Managed (Async a)
async m = managed (Async.withAsync m)

fromHandle :: MonadIO m => Handle -> Stream (Of Text) m ()
fromHandle h = loop
 where
  loop = do
    eof <- liftIO (hIsEOF h)
    unless eof $ do
      line <- liftIO (Text.hGetLine h)
      Streaming.yield line
      loop
