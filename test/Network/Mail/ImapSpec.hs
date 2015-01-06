module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Network.Mail.Imap

runStubTest :: Imap a -> Expectation
runStubTest _ = True `shouldBe` True

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DSL usecases" $ do
    it "Print INBOX" $ runStubTest $ do
      messages <- fetchAll
      return [    show (getUID m)
        ++ " " ++ show (getDate m)
        ++ " " ++ show (getSender m)
        ++ " " ++ show (getSubject m)
        | m <- messages
        ]
