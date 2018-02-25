{-# language LambdaCase   #-}
{-# language RankNTypes   #-}
{-# language ViewPatterns #-}

import Control.Concurrent (newChan, readChan)
import Control.Exception
  (SomeAsyncException(SomeAsyncException), fromException, throwIO, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
  (Result(Error, Success), Value(Object), decodeStrict, fromJSON)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)
import Development.Shake
import Development.Shake.FilePath
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, withArgs)
import System.FSNotify (eventPath, watchTreeChan, withManager)
import System.Posix.Process (executeFile)
import qualified Data.ByteString as ByteString (readFile)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set (fromList)
import qualified System.Info (os)

main :: IO ()
main =
  getArgs >>= \case
    "watch":args ->
      withArgs args $ do
        -- Run 'shake' under a 'try', ignoring synchronous exceptions because
        -- we don't care if the build fails - we want to watch & rebuild
        -- regardless.
        try run >>= \case
          Left (fromException -> Just (SomeAsyncException ex)) -> throwIO ex
          _ -> pure ()

        -- Start watching the filesystem, and rebuild once some file that shake
        -- considered alive changes (it writes these files to ".shake/live").
        cwd <- getCurrentDirectory
        files <- Set.fromList . map ((cwd ++) . ('/':)) . lines <$> readFile ".shake/live"
        withManager $ \manager -> do
          chan <- newChan
          void (watchTreeChan manager "." ((`elem` files) . eventPath) chan)
          void (readChan chan)
        env <- getEnvironment
        executeFile shakefileExe False ("watch":args) (Just env)
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
      , shakeThreads =
          -- Temp workaround for resources not being respected on OSX for some
          -- reason.
          case System.Info.os of
            "linux" -> 4
            _ -> 1
      }

rules :: (forall a. Action a -> Action a) -> Rules ()
rules stack = do
  -- The files we want to build, in no particular order.
  want [daffyExe, shakefileExe]

  phony "clean" $ do
    liftIO (removeFiles "bin" ["//"])
    liftIO (removeFiles "daffy-client/codegen" ["//"])
    liftIO (removeFiles "daffy-server/codegen" ["//"])
    removeFilesAfter ".shake" ["//"]

  phony "deepclean" $ do
    need ["clean"]
    liftIO (removeFiles ".stack-work" ["//*"])
    liftIO (removeFiles "elm-stuff" ["//*"])

  -- Whenever .gitmodules changes, recursively init/update all of them. So,
  -- anything that has *some* submodule should just depend on
  -- ".shake/.gitmodules" as an okay approximation.
  gitmodulesStamp %> \out -> do
    need [".gitmodules"]
    cmd_ "git submodule update --init --recursive"
    writeFile' out ""

  -- Write dependencies from "elm-package.json" out to ".shake/elm-deps".
  elmDeps %> \out -> do
    need ["elm-package.json"]
    deps <-
      parseElmDependencies
        =<< parseElmPackageJson
        =<< liftIO (ByteString.readFile "elm-package.json")
    writeFileChanged out (unlines (map show (HashMap.toList deps)))

  -- Whenever the elm dependencies change, run 'elm package install'.
  elmDepsStamp %> \out -> do
    need [elmDeps]
    cmd_ "elm package install --yes"
    writeFile' out ""

  daffyExe %> \_ -> do
    files <- getDirectoryFiles "" ["daffy-server/src//*.hs", "daffy-server/app//*.hs"]

    let deps =
            "daffy-server/daffy.cabal"
          : daffyJs
          : gitmodulesStamp
          : stackYaml
          : files

    -- It's only necessary to 'stack install' if something besides "daffy.js"
    -- changed.
    needHasChanged deps >>= \case
      [file] | file == daffyJs -> pure ()
      _ -> stack (cmd_ "stack install --fast --flag daffy:development --local-bin-path bin daffy:exe:daffy")

  generateElmTypesExe %> \_ -> do
    files <- getDirectoryFiles "" ["generate-elm-types/src//*.hs", "generate-elm-types/app/Main.hs"]
    need
      ( "generate-elm-types/generate-elm-types.cabal"
      : stackYaml
      : files
      )
    stack (cmd_ "stack install --fast --local-bin-path bin generate-elm-types:exe:generate-elm-types")

  shakefileExe %> \_ -> do
    need
      [ "daffy-shakefile/daffy-shakefile.cabal"
      , "daffy-shakefile/Shakefile.hs"
      , stackYaml
      ]
    stack (cmd_ "stack install --local-bin-path bin daffy-shakefile:exe:Shakefile")

  "daffy-client/codegen/Daffy/Setters.elm" %> \out -> do
    let stub = "daffy-client/src/Daffy/Setters.elm.stub"
    need [generateElmSettersExe, stub]
    mkdir (takeDirectory out)
    cmd_ (FileStdin stub) (FileStdout out) generateElmSettersExe

  "daffy-client/codegen/Daffy/Types.elm" %> \out -> do
    need [generateElmTypesExe]
    mkdir (takeDirectory out)
    cmd_ (FileStdout out) generateElmTypesExe

  daffyJs %> \out -> do
    files <- getDirectoryFiles "" ["daffy-client/src//*.elm"]
    need
      ( "daffy-client/codegen/Daffy/Setters.elm"
      : "daffy-client/codegen/Daffy/Types.elm"
      : elmDepsStamp
      : gitmodulesStamp
      : files
      )
    cmd_ ("elm make --debug daffy-client/src/Main.elm --output=" ++ out)

parseElmPackageJson :: ByteString -> Action (HashMap Text Value)
parseElmPackageJson bytes =
  case decodeStrict bytes of
    Just (Object o) -> pure o
    _ -> fail "Failed to parse elm-package.json"

parseElmDependencies :: HashMap Text Value -> Action (HashMap Text Text)
parseElmDependencies blob =
  case HashMap.lookup (pack "dependencies") blob of
    Just value ->
      case fromJSON value of
        Success deps -> pure deps
        Error s -> fail ("Failed to parse dependencies: " ++ s)
    Nothing -> fail "Failed to parse elm-package.json: missing key 'dependencies'"

-- Like 'mkdir -p'
mkdir :: MonadIO m => FilePath -> m ()
mkdir = liftIO . createDirectoryIfMissing True

daffyExe, daffyJs, elmDeps, elmDepsStamp, generateElmSettersExe,
  generateElmTypesExe, gitmodulesStamp, shakefileExe, stackYaml :: [Char]
daffyExe              = "bin/daffy"
daffyJs               = "daffy-server/codegen/daffy.js"
elmDeps               = ".shake/elm-deps"
elmDepsStamp          = ".shake/elm-deps.stamp"
gitmodulesStamp       = ".shake/gitmodules.stamp"
generateElmSettersExe = "generate-elm-setters/generate-elm-setters.sh"
generateElmTypesExe   = "bin/generate-elm-types"
shakefileExe          = "bin/Shakefile"
stackYaml             = "stack.yaml"
