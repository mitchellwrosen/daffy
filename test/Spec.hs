module Main where

import Daffy.Info (Feature(..), Info(..))

import qualified Daffy.Stats as Stats

import qualified Daffy.Eventlog
import qualified Daffy.Info

import System.Directory
import System.IO.Temp
import System.Process.Typed
import Test.Hspec

import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.IO as Text

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  eventlogSpec
  infoSpec
  statsSpec

eventlogSpec :: Spec
eventlogSpec =
  pure ()

infoSpec :: Spec
infoSpec = do
  describe "parsing '+RTS --info' output" $
    around setup $ do
      it "vanilla" $ \run -> do
        run "" `shouldReturn` Info False Vanilla
      it "profiling" $ \run -> do
        run "-prof" `shouldReturn` Info False Profiling
      it "eventlog" $ \run -> do
        run "-eventlog" `shouldReturn` Info False EventLog
      it "debug" $ \run -> do
        run "-debug" `shouldReturn` Info False Debug
      it "threaded vanilla" $ \run -> do
        run "-threaded" `shouldReturn` Info True Vanilla
      it "profiling" $ \run -> do
        run "-threaded -prof" `shouldReturn` Info True Profiling
      it "eventlog" $ \run -> do
        run "-threaded -eventlog" `shouldReturn` Info True EventLog
      it "debug" $ \run -> do
        run "-threaded -debug" `shouldReturn` Info True Debug
 where
  -- Provide a spec with a function that, given GHC options, compiles and runs a
  -- do-nothing program in a temporary directory and returns its parsed info.
  setup :: ((String -> IO Info) -> IO ()) -> IO ()
  setup run =
    run (\opts -> ghc opts Daffy.Info.run)

statsSpec :: Spec
statsSpec = do
  describe "parsing '-S' output" $ do
    files :: [FilePath] <-
      runIO (listDirectory "test/files/stats")

    forM_ files $ \file ->
      it file $ do
        contents <- Text.readFile ("test/files/stats/" ++ file)
        Stats.parse contents `shouldSatisfy` isRight

    let stats :: String -> String -> IO ()
        stats opts rtsopts =
          ghc opts $ \path -> do
            bytes :: LByteString <-
              readProcessStderr_ (shell (path ++ " +RTS -S " ++ rtsopts))

            let bytes' :: Text
                bytes' = decodeUtf8 (LByteString.toStrict bytes)

            case Stats.parse bytes' of
              Left err ->
                expectationFailure
                  (unlines
                    [ "Parse failure: " ++ err
                    , "Stats output:"
                    , unpack bytes'
                    ])
              Right _ ->
                pure ()

    it "vanilla" (stats "" "")
    it "profiling" (stats "-prof" "")
    it "threaded -N1" (stats "-threaded" "-N1")
    it "threaded -N2" (stats "-threaded" "-N2")

ghc :: String -> (FilePath -> IO a) -> IO a
ghc opts run =
  withTempDirectory "." "daffy" $ \dir -> do
    writeFile (dir ++ "/foo.hs")
      (unlines
        [ "import Control.Applicative (pure)"
        , "main = pure ()"
        ])

    withProcess
      (shell ("ghc -hide-all-packages -package base " ++ opts ++ " foo.hs >/dev/null")
        & setWorkingDir dir)
      checkExitCode

    run (dir ++ "/foo")
