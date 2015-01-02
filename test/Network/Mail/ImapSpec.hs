module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Network.Mail.Imap

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "useless" $ do
    it "empty test" $ do
      True `shouldBe` True
