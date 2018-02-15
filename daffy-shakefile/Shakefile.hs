{-# language LambdaCase #-}
{-# language RankNTypes #-}

import Control.Concurrent (newChan, readChan)
import Development.Shake
import System.Directory (getCurrentDirectory)
import System.FSNotify (eventPath, watchTreeChan, withManager)
import System.Posix.Process (executeFile)

import qualified Data.Set as Set
import qualified System.Environment as Env

main :: IO ()
main = do
  stack <- newResourceIO "stack" 1
  shakeArgs options (mconcat (rules (withResource stack 1)))
 where
  options :: ShakeOptions
  options =
    shakeOptions
      { shakeChange = ChangeModtimeAndDigestInput
      , shakeColor = True
      , shakeLiveFiles = [".shake/live"]
      , shakeProgress = progressSimple
      , shakeThreads = 4
      }

-- The files we want to build, in no particular order.
targets :: [FilePath]
targets =
  [ "bin/daffy"
  , "bin/Shakefile"
  , "daffy-server/codegen/daffy.js"
  ]

rules :: (forall a. Action a -> Action a) -> [Rules ()]
rules stack =
  [ want targets

  , phony "watch" $ do
      runAfter $ do
        cwd <- getCurrentDirectory
        files <-
          Set.delete "watch" . Set.fromList . map ((cwd ++) . ('/':)) . lines
            <$> readFile ".shake/live"
        withManager $ \mgr -> do
          chan <- newChan
          _ <- watchTreeChan mgr "." ((`elem` files) . eventPath) chan
          _ <- readChan chan
          pure ()
        env <- Env.getEnvironment
        executeFile "bin/Shakefile" False ["watch"] (Just env)

      need targets

  , "bin/daffy" %> \_ -> do
      files <- getDirectoryFiles "" ["daffy-server/src//*.hs", "daffy-server/app//*.hs"]
      need ("stack.yaml" : "daffy-server/daffy.cabal" : files)
      stack (cmd_ "stack install --fast --local-bin-path bin daffy:exe:daffy")

  , "bin/Shakefile" %> \_ -> do
      need ["daffy-shakefile/daffy-shakefile.cabal", "daffy-shakefile/Shakefile.hs"]
      stack (cmd_ "stack install --local-bin-path bin daffy-shakefile:exe:Shakefile")

  , "bin/daffy-elm-codegen" %> \_ -> do
      files <- getDirectoryFiles "" ["daffy-elm-codegen/src//*.hs", "daffy-elm-codegen/app//*.hs"]
      need ("stack.yaml" : "daffy-elm-codegen/daffy-elm-codegen.cabal" : files)
      stack (cmd_ "stack install --fast --local-bin-path bin daffy-elm-codegen:exe:daffy-elm-codegen")

  , "daffy-client/codegen/DaffyTypes.elm" %> \out -> do
      need ["bin/daffy-elm-codegen"]
      cmd_ (FileStdout out) "bin/daffy-elm-codegen"

  , "daffy-server/codegen/daffy.js" %> \out -> do
      files <- getDirectoryFiles "" ["daffy-client/src//*.elm"]
      need ("elm-package.json" : "daffy-client/codegen/DaffyTypes.elm" : files)
      cmd_ ("elm-make daffy-client/src/Main.elm --output=" ++ out)
  ]
