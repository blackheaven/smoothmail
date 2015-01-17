module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.ImapStub

prettyPrintCurrentDirectory :: Imap [String]
prettyPrintCurrentDirectory = do
    messages <- searchAll >>= fetchHeader
    return [    show (extractUID $ getUID $ h)
           ++ " " ++ show (getDate h)
           ++ " " ++ show (getSender h)
           ++ " " ++ show (getSubject h)
           | h <- messages
           ]

prettyPrintFirstMailOfCurrentDirectory :: Imap [String]
prettyPrintFirstMailOfCurrentDirectory = do
    messages <- searchAll >>= fetchHeader . take 1
    return [    show (extractUID $ getUID $ h)
           ++ " " ++ show (getDate h)
           ++ " " ++ show (getSender h)
           ++ " " ++ show (getSubject h)
           | h <- messages
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
    it "Print INBOX" $ do
      runStubTest prettyPrintFirstMailOfCurrentDirectory `shouldBe` [
                                                           "1 2015-01-01 10:10 S1 T1"
                                                         ]
