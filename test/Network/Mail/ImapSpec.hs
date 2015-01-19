module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.ImapStub

prettyPrintCurrentDirectory :: Imap [String]
prettyPrintCurrentDirectory =
    getAllMails fetchHeader >>= prettyPrintHeaders

prettyPrintFirstMailOfCurrentDirectory :: Imap [String]
prettyPrintFirstMailOfCurrentDirectory =
    getAllMails (fetchHeader . take 1) >>= prettyPrintHeaders

getAllMails :: ([UID] -> Imap (Maybe a)) -> Imap (Maybe a)
getAllMails f = searchAll >>= maybe (return Nothing) f

prettyPrintHeaders :: Maybe [Header] -> Imap [String]
prettyPrintHeaders messages = return
           [         show (extractUID $ getUID $ h)
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
    describe "Print INBOX" $ do
      it "Print all mails" $ do
        runStubTest prettyPrintCurrentDirectory `shouldBe` [
                                                             "1 2015-01-01 10:10 S1 T1"
                                                           , "2 2015-02-03 21:12 S2 T2"
                                                           ]
      it "Print the first mail" $ do
        runStubTest prettyPrintFirstMailOfCurrentDirectory `shouldBe` [
                                                             "1 2015-01-01 10:10 S1 T1"
                                                         ]
