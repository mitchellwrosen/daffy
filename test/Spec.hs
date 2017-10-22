module Main where

import Daffy.Stats

import Data.Either
import Test.Hspec

import qualified Data.ByteString as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing -S files" $ do
    it "files/1.txt" $ do
      contents <- ByteString.readFile "test/files/1.txt"
      parseStats contents `shouldSatisfy` isRight

    it "files/2.txt" $ do
      contents <- ByteString.readFile "test/files/2.txt"
      parseStats contents `shouldSatisfy` isRight
