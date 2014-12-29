module Data.Network.ImapSpec (main, spec) where

import Test.Hspec
import Data.Network.Imap

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "useless" $ do
    it "empty test" $ do
      True `shouldBe` True
