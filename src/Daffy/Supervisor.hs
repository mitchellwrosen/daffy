-- | Crudely supervise a collection of threads: spawn a thread with 'spawn', and
-- wait for the whole collection to finish with 'wait'.
--
-- If any thread throws an exception, all other threads are killed, then the
-- supervisor re-throws the original exception.
module Daffy.Supervisor
  ( Supervisor
  , new
  , spawn
  , Daffy.Supervisor.wait
  ) where

newtype Supervisor
  = Supervisor (IORef [Async ()])

new :: MonadIO m => m Supervisor
new =
  io (Supervisor <$> newIORef [])

spawn :: MonadIO m => Supervisor -> IO () -> m ()
spawn (Supervisor ref) action = io $ do
  worker :: Async () <-
    async action
  modifyIORef' ref (worker :)

wait :: MonadIO m => Supervisor -> m ()
wait (Supervisor ref) = io $ do
  workers :: [Async ()] <-
    readIORef ref
  go workers
 where
  go = \case
    [] ->
      pure ()
    w:ws ->
      waitCatch w >>= \case
        Left ex -> do
          mapM_ cancel ws
          throw ex
        Right () ->
          go ws
