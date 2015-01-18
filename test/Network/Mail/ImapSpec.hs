module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.ImapStub

prettyPrintCurrentDirectory :: Imap [String]
prettyPrintCurrentDirectory = do
    messages <- searchAll >>= maybe (return Nothing) fetchHeader
    return [    show (extractUID $ getUID $ h)
           ++ " " ++ getDate h
           ++ " " ++ getSender h
           ++ " " ++ getSubject h
           | h <- maybe [] id messages
           ]

prettyPrintFirstMailOfCurrentDirectory :: Imap [String]
prettyPrintFirstMailOfCurrentDirectory = do
    messages <- searchAll >>= maybe (return Nothing) (fetchHeader . take 1)
    return [         show (extractUID $ getUID $ h)
           ++ " " ++ getDate h
           ++ " " ++ getSender h
           ++ " " ++ getSubject h
           | h <- maybe [] id messages
           ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DSL usecases" $ do
    it "Print all the INBOX" $ do
      runStubTest prettyPrintCurrentDirectory `shouldBe` [
                                                           "1 2015-01-01 10:10 S1 T1"
                                                         , "2 2015-02-03 21:12 S2 T2"
                                                         ]
    it "Print first Mail of the INBOX" $ do
      runStubTest prettyPrintFirstMailOfCurrentDirectory `shouldBe` [
                                                           "1 2015-01-01 10:10 S1 T1"
                                                         ]
