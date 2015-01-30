module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Data.List (intercalate)
import Network.Mail
import Network.Mail.Imap
import Network.Mail.ImapStub

prettyPrintCurrentDirectory :: Imap (Maybe [String])
prettyPrintCurrentDirectory =
    getAllMails fetchHeader >>= prettyPrintHeaders

prettyPrintFirstMailOfCurrentDirectory :: Imap (Maybe [String])
prettyPrintFirstMailOfCurrentDirectory =
    getAllMails (fetchHeader . take 1) >>= prettyPrintHeaders

getAllMails :: ([UID] -> Imap (Maybe a)) -> Imap (Maybe a)
getAllMails f = searchAll >>= maybe (return Nothing) f

prettyPrintHeaders :: Maybe [Header] -> Imap (Maybe [String])
prettyPrintHeaders = return . fmap (fmap showHeader)
  where showHeader h = intercalate " " $ fmap ($ h)
                        [show . extractUID . getUID, getDate, getSender, getSubject]


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DSL usecases" $ do
    describe "select" $ do
      describe "'INBOX' directory" $ do
        it "Print all mails" $ do
          runStubTest prettyPrintCurrentDirectory
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
        it "Print the first mail" $ do
          runStubTest prettyPrintFirstMailOfCurrentDirectory
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1"]
        it "Print all mails after going into a folder and going up" $ do
          runStubTest (select "Personal" >> select ".." >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
      describe "'Personal' directory" $ do
        it "Print all mails" $ do
          runStubTest (select "Personal" >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["3 2015-04-05 12:34 S2 T1"]
      describe "Unknown directory" $ do
        it "Print nothing" $ do
          runStubTest (select "Unknown" >> prettyPrintCurrentDirectory)
          `shouldBe` Nothing
    describe "create" $ do
        it "Create a non-existing directory should be true" $ do
          runStubTest (create "New")
          `shouldBe` True
        it "Create an existing directory should be false" $ do
          runStubTest (create "Personal")
          `shouldBe` False
    describe "delete" $ do
        it "Delete an existing directory should be true" $ do
          runStubTest (delete "Personal")
          `shouldBe` True
        it "Delete a non-existing directory should be false" $ do
          runStubTest (delete "Unknown")
          `shouldBe` False
