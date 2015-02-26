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
        it "Print all mails after going to itself" $ do
          runStubTest (select "." >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
        it "Print all mails after going into a folder, to this one and going up" $ do
          runStubTest (select "Personal" >> select "." >> select ".." >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
        it "Print all mails after going to itself, then into a folder, to this one and going up" $ do
          runStubTest (select "Personal" >> select "." >> select ".." >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
      describe "'Personal' directory" $ do
        it "Print all mails" $ do
          runStubTest (select "Personal" >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["3 2015-04-05 12:34 S2 T1"]
        it "Print all mails after going to 'Personal' then to itself" $ do
          runStubTest (select "Personal" >> select "." >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["3 2015-04-05 12:34 S2 T1"]
        it "Print all mails after going to itself then to 'Personal'" $ do
          runStubTest (select "." >> select "Personal" >> prettyPrintCurrentDirectory)
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
    describe "rename" $ do
        it "Rename an existing directory with a non-existing directory name should be true" $ do
          runStubTest (select "Personal" >> rename "Personal_new")
          `shouldBe` True
        it "Rename an existing directory with an existing directory name should be false" $ do
          runStubTest (select "Personal" >> rename "Work")
          `shouldBe` False
    describe "delete" $ do
        it "Delete an existing directory should be true" $ do
          runStubTest (delete "Personal")
          `shouldBe` True
        it "Delete a non-existing directory should be false" $ do
          runStubTest (delete "Unknown")
          `shouldBe` False
    describe "subscribe" $ do
        it "Subscribe to an existing directory should be true" $ do
          runStubTest (subscribe "Personal")
          `shouldBe` True
        it "Subscribe to a non-existing directory should be false" $ do
          runStubTest (subscribe "Unknown")
          `shouldBe` False
    describe "unsubscribe" $ do
        it "Unsubscribe to an existing directory should be true" $ do
          runStubTest (unsubscribe "Personal")
          `shouldBe` True
        it "Unsubscribe to a non-existing directory should be false" $ do
          runStubTest (unsubscribe "Unknown")
          `shouldBe` False
    describe "list" $ do
        it "list 'INBOX' directory should return 'Personal' and 'Work'" $ do
          runStubTest (list (Left "."))
          `shouldBe` Just ["Personal", "Work"]
        it "list 'Personal' directory should return en empty list" $ do
          runStubTest (list (Left "Personal"))
          `shouldBe` Just []
    describe "lsub" $ do
        it "lsub 'INBOX' directory should return 'Personal' and 'Work'" $ do
          runStubTest (lsub (Left "."))
          `shouldBe` Just ["Personal", "Work"]
        it "lsub 'Personal' directory should return en empty list" $ do
          runStubTest (lsub (Left "Personal"))
          `shouldBe` Just []
    describe "expunge" $ do
        it "should be true" $ do
          runStubTest expunge
          `shouldBe` True
        it "go to 'Personal' folder and run expunge should be false" $ do
          runStubTest (select "Personal" >> expunge)
          `shouldBe` False
    describe "check" $ do
        it "Check and print all mails of 'INBOX'" $ do
          runStubTest (check >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
        it "Check and list 'INBOX' directory should return 'Personal' and 'Work'" $ do
          runStubTest (check >> list (Left "."))
          `shouldBe` Just ["Personal", "Work"]
        it "Check, go to 'Personal' folder and print all mails" $ do
          runStubTest (check >> select "Personal" >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["3 2015-04-05 12:34 S2 T1"]
