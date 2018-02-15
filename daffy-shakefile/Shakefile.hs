#!/usr/bin/env stack
-- stack script --resolver lts-10.5

{-# language LambdaCase #-}

import Development.Shake
import System.Exit (exitFailure)
import System.Posix.Process (executeFile)

import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Process as Proc

main :: IO ()
main =
  Env.getProgName >>= \case
    "Shakefile" ->
      shakeArgs options (mconcat rules)
    "Shakefile.hs" -> do
      unlessM (Dir.doesFileExist "bin/Shakefile")
        (Proc.callCommand "stack install --local-bin-path bin daffy-shakefile:exe:Shakefile")
      args <- Env.getArgs
      env <- Env.getEnvironment
      executeFile "bin/Shakefile" False (args ++ ["+RTS", "-I0", "-qb", "-qg"]) (Just env)
    _ ->
      exitFailure
 where
  options :: ShakeOptions
  options =
    shakeOptions
      { shakeChange = ChangeModtimeAndDigestInput
      , shakeColor = True
      , shakeThreads = 4
      }

rules :: [Rules ()]
rules =
  [ -- The files we want to build, in no particular order.
    want
      [ "bin/daffy"
      , "bin/Shakefile"
      , "daffy-server/codegen/daffy.js"
      ]

  , "bin/daffy" %> \_ -> do
      hsFiles <- getDirectoryFiles "" ["daffy-server//*.hs"]
      need ("stack.yaml" : "daffy-server/daffy.cabal" : hsFiles)
      cmd_ "stack install --fast --local-bin-path bin daffy:exe:daffy"

  , "bin/daffy-elm-codegen" %> \_ -> do
      hsFiles <- getDirectoryFiles "" ["daffy-elm-codegen//*.hs"]
      need ("stack.yaml" : "daffy-elm-codegen/daffy-elm-codegen.cabal" : hsFiles)
      cmd_ "stack install --fast --local-bin-path bin daffy-elm-codegen:exe:daffy-elm-codegen"

  , "bin/Shakefile" %> \_ -> do
      need ["stack.yaml", "daffy-shakefile/daffy-shakefile.cabal", "daffy-shakefile/Shakefile.hs"]
      cmd_ "stack install --local-bin-path bin daffy-shakefile:exe:Shakefile"

  , "daffy-client/codegen/DaffyTypes.elm" %> \out -> do
      need ["bin/daffy-elm-codegen"]
      cmd_ (FileStdout out) "bin/daffy-elm-codegen"

  , "daffy-server/codegen/daffy.js" %> \out -> do
      elmFiles <- getDirectoryFiles "" ["daffy-client/src/*.elm"]
      need ("elm-package.json" : "daffy-client/codegen/DaffyTypes.elm" : elmFiles)
      cmd_ ("elm-make daffy-client/src/Main.elm --output=" ++ out)
  ]

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM x y =
  x >>= \case
    True ->
      pure ()
    False ->
      y
