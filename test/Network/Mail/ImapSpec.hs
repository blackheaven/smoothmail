module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Network.Mail.Imap
import Network.Mail.ImapStub

prettyPrintCurrentDirectory :: Imap [String]
prettyPrintCurrentDirectory = do
    messages <- searchAll >>= fetchAll
    return [    show (getUID m)
           ++ " " ++ show (getDate m)
           ++ " " ++ show (getSender m)
           ++ " " ++ show (getSubject m)
           | m <- messages
           ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DSL usecases" $ do
    it "Print INBOX" $ do
      runStubTest prettyPrintCurrentDirectory `shouldBe` [
                                                           "1 2015-01-01 10:10 S1 T1"
                                                         , "2 2015-02-03 21:12 S1 T2"
                                                         ]
