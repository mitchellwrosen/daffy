{-# language LambdaCase #-}
{-# language RankNTypes #-}

import Control.Concurrent (newChan, readChan)
import Data.Function ((&))
import Development.Shake
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, withArgs)
import System.FSNotify (eventPath, watchTreeChan, withManager)
import System.Posix.Process (executeFile)
import UnliftIO (tryAny)

import qualified Data.Set as Set

main :: IO ()
main =
  getArgs >>= \case
    ["watch"] ->
      withArgs [] $ do
        _ <- tryAny run
        cwd <- getCurrentDirectory
        files <-
          Set.fromList . map ((cwd ++) . ('/':)) . lines
            <$> readFile ".shake/live"
        withManager $ \mgr -> do
          chan <- newChan
          _ <- watchTreeChan mgr "." ((`elem` files) . eventPath) chan
          _ <- readChan chan
          pure ()
        env <- getEnvironment
        executeFile "bin/Shakefile" False ["watch"] (Just env)
    _ -> do
      run
 where
  run :: IO ()
  run = do
    -- Only run one copy of stack at a time.
    stack <- newResourceIO "stack" 1
    shakeArgs options (rules (withResource stack 1))

  options :: ShakeOptions
  options =
    shakeOptions
      { shakeChange = ChangeModtimeAndDigestInput
      , shakeColor = True
      , shakeLiveFiles = [".shake/live"]
      , shakeProgress = progressSimple
      , shakeThreads = 4
      }

rules :: (forall a. Action a -> Action a) -> Rules ()
rules stack = do
  action $ do
    Stdout xs <- cmd "git config -f .gitmodules --get-regexp path$"
    let submodules = xs & lines & map ((++ "/.git") . (!! 1) . words)

    -- The files we want to build, in no particular order.
    need
      ( "bin/daffy"
      : "bin/Shakefile"
      : "daffy-server/codegen/daffy.js"
      : submodules
      )

  "*//.git" %> \_ ->
    cmd_ "git submodule update --init --recursive"

  "bin/daffy" %> \_ -> do
    _ <- getEnv "DAFFY_DEV"
    files <- getDirectoryFiles "" ["daffy-server/src//*.hs", "daffy-server/app//*.hs"]
    need ("stack.yaml" : "daffy-server/daffy.cabal" : files)
    stack (cmd_ "stack install --fast --local-bin-path bin daffy:exe:daffy")

  "bin/Shakefile" %> \_ -> do
    need ["daffy-shakefile/daffy-shakefile.cabal", "daffy-shakefile/Shakefile.hs"]
    stack (cmd_ "stack install --local-bin-path bin daffy-shakefile:exe:Shakefile")

  "bin/daffy-elm-codegen" %> \_ -> do
    files <- getDirectoryFiles "" ["daffy-elm-codegen/src//*.hs", "daffy-elm-codegen/app//*.hs"]
    need ("stack.yaml" : "daffy-elm-codegen/daffy-elm-codegen.cabal" : files)
    stack (cmd_ "stack install --fast --local-bin-path bin daffy-elm-codegen:exe:daffy-elm-codegen")

  "daffy-client/codegen/DaffyTypes.elm" %> \out -> do
    need ["bin/daffy-elm-codegen"]
    cmd_ (FileStdout out) "bin/daffy-elm-codegen"

  "daffy-server/codegen/daffy.js" %> \out -> do
    files <- getDirectoryFiles "" ["daffy-client/src//*.elm"]
    need ("elm-package.json" : "daffy-client/codegen/DaffyTypes.elm" : files)
    cmd_ ("elm-make --debug daffy-client/src/Main.elm --output=" ++ out)
