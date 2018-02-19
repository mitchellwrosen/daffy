{-# language LambdaCase #-}
{-# language RankNTypes #-}

import Control.Concurrent (newChan, readChan)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Development.Shake
import Development.Shake.FilePath
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, withArgs)
import System.FSNotify (eventPath, watchTreeChan, withManager)
import System.Posix.Process (executeFile)
import UnliftIO (tryAny)

import qualified Data.Aeson as Aeson (Value, json, withObject)
import qualified Data.Aeson.Internal as Aeson (iparse)
import qualified Data.Aeson.Parser as Aeson (eitherDecodeStrictWith)
import qualified Data.Aeson.Types as Aeson (Parser, parseField)
import qualified Data.HashMap.Strict as HashMap (toList)
import qualified Data.Set as Set (fromList)

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
  -- The files we want to build, in no particular order.
  want
    [ ".shake/gitmodules"
    , "bin/daffy"
    , "bin/Shakefile"
    ]

  -- Whenever .gitmodules changes, recursively init/update all of them.
  ".shake/gitmodules" %> \out -> do
    Stdout xs <- cmd "git config -f .gitmodules --get-regexp path$"
    let submodules = xs & lines & map ((</> ".git") . (!! 1) . words)
    need (".gitmodules" : submodules)
    cmd_ "git submodule update --init --recursive"
    writeFile' out ""

  -- Snip out just the dependencies from "elm-package.json" and write them to
  -- ".shake/elm-deps.txt".
  ".shake/elm-deps.txt" %> \out -> do
    bytes <- encodeUtf8 . pack <$> readFile' "elm-package.json"
    let parser :: Aeson.Value -> Aeson.Parser (HashMap Text Text)
        parser =
          Aeson.withObject "elm-package.json" $ \o ->
            Aeson.parseField o (pack "dependencies")
    case Aeson.eitherDecodeStrictWith Aeson.json (Aeson.iparse parser) bytes of
      Left (_, err) ->
        fail err
      Right deps ->
        writeFileChanged out (show (HashMap.toList deps))

  -- Write an empty "elm-deps.stamp" file whenever the dependencies in
  -- "elm-package.json" change.
  ".shake/elm-deps.stamp" %> \out -> do
    need [".shake/elm-deps.txt"]
    cmd_ "elm package install --yes"
    writeFile' out ""

  "bin/daffy" %> \_ -> do
    files <- getDirectoryFiles "" ["daffy-server/src//*.hs", "daffy-server/app//*.hs"]
    need ("stack.yaml" : "daffy-server/daffy.cabal" : "daffy-server/codegen/daffy.js" : files)
    stack (cmd_ "stack install --fast --flag daffy:development --local-bin-path bin daffy:exe:daffy")

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
    need (".shake/elm-deps.stamp" : "elm-package.json" : "daffy-client/codegen/DaffyTypes.elm" : files)
    cmd_ ("elm-make --debug daffy-client/src/Main.elm --output=" ++ out)
