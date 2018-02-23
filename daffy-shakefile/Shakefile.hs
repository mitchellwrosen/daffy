{-# language LambdaCase   #-}
{-# language RankNTypes   #-}
{-# language ViewPatterns #-}

import Control.Concurrent (newChan, readChan)
import Control.Exception (SomeAsyncException(SomeAsyncException), fromException, throwIO, try)
import Data.Functor (void)
import Development.Shake
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, withArgs)
import System.FSNotify (eventPath, watchTreeChan, withManager)
import System.Posix.Process (executeFile)

import qualified Data.Set as Set (fromList)

main :: IO ()
main =
  getArgs >>= \case
    "watch":args ->
      withArgs args $ do
        -- Run 'shake' under a 'try', ignoring synchronous exceptions because
        -- we don't care if it fails.
        try run >>= \case
          Left (fromException -> Just (SomeAsyncException ex)) -> throwIO ex
          _ -> pure ()

        -- Start watching the filesystem, and rebuild once some file that shake
        -- considered alive changes (it writes these files to .shake/live).
        cwd <- getCurrentDirectory
        files <- Set.fromList . map ((cwd ++) . ('/':)) . lines <$> readFile ".shake/live"
        withManager $ \manager -> do
          chan <- newChan
          void (watchTreeChan manager "." ((`elem` files) . eventPath) chan)
          void (readChan chan)
        env <- getEnvironment
        executeFile "bin/Shakefile" False ("watch":args) (Just env)
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
  -- The files we want to build, in no particular order.
  want
    [ ".shake/.gitmodules"
    , "bin/daffy"
    , "bin/daffy-elm-codegen"
    , "bin/Shakefile"
    , "daffy-client/codegen/DaffyTypes.elm"
    , "daffy-server/codegen/daffy.js"
    ]

  phony "clean" $ do
    liftIO (removeFiles "bin" ["//"])
    liftIO (removeFiles "daffy-client/codegen" ["//"])
    liftIO (removeFiles "daffy-server/codegen" ["//"])
    removeFilesAfter ".shake" ["//"]

  phony "deepclean" $ do
    need ["clean"]
    liftIO (removeFiles ".stack-work" ["//*"])
    liftIO (removeFiles "elm-stuff" ["//*"])

  -- Whenever .gitmodules changes, recursively init/update all of them.
  ".shake/.gitmodules" %> \out -> do
    need [".gitmodules"]
    cmd_ "git submodule update --init --recursive"
    writeFile' out ""

  -- Whenever elm-package.json changes, run "elm package install --yes". If
  -- something actually happened, update the modtime of .shake/elm-package.json.
  ".shake/elm-package.json" %> \out -> do
    need ["elm-package.json"]
    Stdout output <- cmd "elm package install --yes"
    case output of
      "Packages configured successfully!" -> pure ()
      _ -> writeFile' out ""

  "bin/daffy" %> \_ -> do
    -- Require "daffy.js" to be built before building the server, but don't
    -- actually add it as a dependency, because (in development mode) we don't
    -- need to rebuild the server if "daffy.js" changes.
    doesFileExist "daffy-server/codegen/daffy.js" >>= \case
      False -> do
        -- Touch the file because in order to build "daffy.js", we need to build
        -- and run 'daffy-elm-codegen', which will build 'daffy' the library,
        -- which will require "daffy.js" to exist (it's a data-file).
        liftIO (createDirectoryIfMissing False "daffy-server/codegen")
        liftIO (appendFile "daffy-server/codegen/daffy.js" "")
      True -> pure ()
    orderOnly ["daffy-server/codegen/daffy.js"]
    files <- getDirectoryFiles "" ["daffy-server/src//*.hs", "daffy-server/app//*.hs"]
    need ("stack.yaml" : "daffy-server/daffy.cabal" : files)
    stack (cmd_ "stack install --fast --flag daffy:development --local-bin-path bin daffy:exe:daffy")

  "bin/daffy-elm-codegen" %> \_ -> do
    files <- getDirectoryFiles "" ["daffy-elm-codegen/src//*.hs", "daffy-elm-codegen/app//*.hs"]
    need ("stack.yaml" : "daffy-elm-codegen/daffy-elm-codegen.cabal" : files)
    stack (cmd_ "stack install --fast --local-bin-path bin daffy-elm-codegen:exe:daffy-elm-codegen")

  "bin/Shakefile" %> \_ -> do
    need ["stack.yaml", "daffy-shakefile/daffy-shakefile.cabal", "daffy-shakefile/Shakefile.hs"]
    stack (cmd_ "stack install --local-bin-path bin daffy-shakefile:exe:Shakefile")

  "daffy-client/codegen/DaffyTypes.elm" %> \out -> do
    need ["bin/daffy-elm-codegen"]
    cmd_ (FileStdout out) "bin/daffy-elm-codegen"

  "daffy-server/codegen/daffy.js" %> \out -> do
    files <- getDirectoryFiles "" ["daffy-client/src//*.elm"]
    need (".shake/elm-package.json" : "daffy-client/codegen/DaffyTypes.elm" : files)
    cmd_ ("elm make --debug daffy-client/src/Main.elm --output=" ++ out)
