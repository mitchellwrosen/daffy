module Daffy.Process
  ( Output(..)
  , ProcessSnapshot(..)
  , spawn
  , snapshot
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async (waitSTM, withAsync)
import Control.Concurrent.STM
import Control.Monad (join, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, MonadManaged, managed, using, with)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Streaming (Stream)
import Streaming.Prelude (Of)
import System.Exit (ExitCode)
import System.IO (Handle, hIsEOF)
import System.Process
  (CmdSpec(ShellCommand), CreateProcess(..), StdStream(CreatePipe, NoStream),
    createProcess, waitForProcess)

import qualified Data.Text.IO as Text
import qualified Streaming.Prelude as Streaming

data Output
  = Stdout Text
  | Stderr Text
  deriving (Eq, Show)

-- | Spawn a shell command and stream its output, terminating with the process's
-- exit code.
spawn :: forall m.  MonadManaged m => String -> Stream (Of Output) m ExitCode
spawn path = do
  (Nothing, Just hout, Just herr, ph) <- liftIO (createProcess spec)

  output :: TQueue Output <-
    liftIO newTQueueIO

  let enqueueStdout :: IO ()
      enqueueStdout =
        Streaming.mapM_
          (atomically . writeTQueue output . Stdout)
          (fromHandle hout)

  let enqueueStderr :: IO ()
      enqueueStderr =
        Streaming.mapM_
          (atomically . writeTQueue output . Stderr)
          (fromHandle herr)

  -- Three concurrent actions: reading stdout, reading stderr, and waiting
  -- for the process to end.
  a1 <- lift (using (managed (withAsync enqueueStdout)))
  a2 <- lift (using (managed (withAsync enqueueStderr)))
  a3 <- lift (using (managed (withAsync (waitForProcess ph))))

  let loop :: Stream (Of Output) m ExitCode
      loop = join (liftIO (atomically (act1 <|> act2)))
       where
        act1 :: STM (Stream (Of Output) m ExitCode)
        act1 = do
          out <- readTQueue output
          pure $ do
            Streaming.yield out
            loop

        act2 :: STM (Stream (Of Output) m ExitCode)
        act2 = do
          waitSTM a1
          waitSTM a2
          code <- waitSTM a3
          pure (pure code)

  loop
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

-- | The snapshot of a (still running?) process: either its output so far and
-- a stream of the remaining output terminated by an exit code, or all of its
-- output and its exit code.
data ProcessSnapshot
  = ProcessRunning [Output] (Stream (Of Output) IO ExitCode)
  | ProcessFinished [Output] ExitCode

snapshot :: Stream (Of Output) Managed ExitCode -> IO (IO ProcessSnapshot)
snapshot output = do
  outputChan :: TChan (Maybe Output) <-
    newBroadcastTChanIO

  seenVar :: TVar [Output] <-
    newTVarIO []

  codeVar :: TMVar ExitCode <-
    newEmptyTMVarIO

  -- FIXME: Ehh... better handling of exceptions
  _ <-
    forkIO $ do
      code :: ExitCode <-
        with
          (Streaming.mapM_
            (\out ->
              liftIO . atomically $ do
                writeTChan outputChan (Just out)
                modifyTVar' seenVar (out:))
            output)
          pure

      atomically $ do
        writeTChan outputChan Nothing
        putTMVar codeVar code

  let act1 :: STM ProcessSnapshot
      act1 = do
        code <- readTMVar codeVar
        seen <- readTVar seenVar
        pure (ProcessFinished seen code)

  let act2 :: STM ProcessSnapshot
      act2 = do
        seen :: [Output] <-
          readTVar seenVar

        chan :: TChan (Maybe Output) <-
          dupTChan outputChan

        let stream :: Stream (Of Output) IO ExitCode
            stream =
              liftIO (atomically (readTChan chan)) >>= \case
                Just out -> do
                  Streaming.yield out
                  stream
                Nothing -> liftIO (atomically (readTMVar codeVar))

        pure (ProcessRunning seen stream)

  pure (atomically (act1 <|> act2))

fromHandle :: MonadIO m => Handle -> Stream (Of Text) m ()
fromHandle h = loop
 where
  loop = do
    eof <- liftIO (hIsEOF h)
    unless eof $ do
      line <- liftIO (Text.hGetLine h)
      Streaming.yield line
      loop
