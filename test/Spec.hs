module Main where

import Daffy.Stats

import System.Directory
import Test.Hspec

import qualified Data.Text.IO as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing -S files" $ do
    files :: [FilePath] <-
      runIO (listDirectory "test/files/stats")

    forM_ files $ \file ->
      it file $ do
        contents <- Text.readFile ("test/files/stats/" ++ file)
        parseStats contents `shouldSatisfy` isRight
