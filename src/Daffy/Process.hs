module Daffy.Process
  ( Output(..)
  , spawn
  ) where

import Control.Applicative
import Control.Exception.Safe (SomeException, try)
import Control.Concurrent.Async (waitSTM, withAsync)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue
  (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad (join, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (MonadManaged, managed, using)
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

-- Spawn a shell command and stream its output, terminating with any exception
-- thrown, or the process's exit code.
spawn
  :: forall m.
     MonadManaged m
  => String -> Stream (Of Output) m (Either SomeException ExitCode)
spawn path = do
  liftIO (try (createProcess spec)) >>= \case
    Left ex -> pure (Left ex)
    Right (Nothing, Just hout, Just herr, ph) -> do
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

      Right <$> loop
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

fromHandle :: MonadIO m => Handle -> Stream (Of Text) m ()
fromHandle h = loop
 where
  loop = do
    eof <- liftIO (hIsEOF h)
    unless eof $ do
      line <- liftIO (Text.hGetLine h)
      Streaming.yield line
      loop
