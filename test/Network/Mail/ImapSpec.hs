module Network.Mail.ImapSpec (main, spec) where

import Test.Hspec
import Data.List (intercalate)
import Network.Mail
import Network.Mail.Imap
import Network.Mail.Imap.Types
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
        it "DirectoryDescription with two mails" $ do
          runStubTest (select ".")
          `shouldBe` Just (DirectoryDescription 0 2 0)
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
        it "DirectoryDescription with only one mail" $ do
          runStubTest (select "Personal")
          `shouldBe` Just (DirectoryDescription 0 1 0)
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
        it "Print the cotent of the previous valid Directory, here 'INBOX'" $ do
          runStubTest (select "Unknown" >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
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
        it "Expunge 'INBOX' directory should be true" $ do
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
        it "Check and rename an existing directory with a non-existing directory name should be true" $ do
          runStubTest (check >> select "Personal" >> rename "Personal_new")
          `shouldBe` True
        it "Check and subscribe to an existing directory should be true" $ do
          runStubTest (check >> subscribe "Personal")
          `shouldBe` True
        it "Check and unsubscribe to an existing directory should be true" $ do
          runStubTest (check >> unsubscribe "Personal")
          `shouldBe` True
        it "Check and list 'INBOX' directory should return 'Personal' and 'Work'" $ do
          runStubTest (check >> list (Left "."))
          `shouldBe` Just ["Personal", "Work"]
        it "Check and lsub 'INBOX' directory should return 'Personal' and 'Work'" $ do
          runStubTest (check >> lsub (Left "."))
          `shouldBe` Just ["Personal", "Work"]
        it "Check and expunge should be true" $ do
          runStubTest (check >> expunge)
          `shouldBe` True
    describe "examine" $ do
        it "Examine a directory and print all mails should print those of 'INBOX'" $ do
          runStubTest (examine "Personal" >> prettyPrintCurrentDirectory)
          `shouldBe` Just ["1 2015-01-01 10:10 S1 T1", "2 2015-02-03 21:12 S2 T2"]
        it "Examine a directory should return a DirectoryDescription with only one mail" $ do
          runStubTest (examine "Personal")
          `shouldBe` Just (DirectoryDescription 0 1 0)
        it "Examine itself should return a DirectoryDescription with two mails" $ do
          runStubTest (examine ".")
          `shouldBe` Just (DirectoryDescription 0 2 0)
        it "Examine an unknown directory should return Nothing" $ do
          runStubTest (examine "Unknown")
          `shouldBe` Nothing
    describe "noop" $ do
        it "Noop on 'INBOX' should return a DirectoryDescription with two mails" $ do
          runStubTest noop
          `shouldBe` DirectoryDescription 0 2 0
        it "Noop on 'Personal' should return a DirectoryDescription with one mail" $ do
          runStubTest (select "Personal" >> noop)
          `shouldBe` DirectoryDescription 0 1 0
        it "Noop on an unknown directory should return the DirectoryDescription of 'INBOX', ie. two mails" $ do
          runStubTest (select "Unknown" >> noop)
          `shouldBe` DirectoryDescription 0 2 0
        it "Noop on an unknown directory after going in a know one should return the DirectoryDescription of the known directory, ie. one mail" $ do
          runStubTest (select "Personal" >> select "Unknown" >> noop)
          `shouldBe` DirectoryDescription 0 1 0
        it "Noop on an unknown directory on the parent directory after going in a know one should return the DirectoryDescription of the known directory, ie. one mail" $ do
          runStubTest (select "Personal" >> select "../Unknown" >> noop)
          `shouldBe` DirectoryDescription 0 1 0
    describe "status" $ do
      describe "unseen item" $ do
        it "status on 'INBOX' should return with two mails" $ do
          runStubTest (status "." SQUnseen)
          `shouldBe` Just 2
        it "status on 'Personal' should return with two mails" $ do
          runStubTest (status "Personal" SQUnseen)
          `shouldBe` Just 2
      describe "messages item" $ do
        it "status on 'INBOX' should return with two mails" $ do
          runStubTest (status "." SQMessages)
          `shouldBe` Just 2
        it "status on 'Personal' should return with two mails" $ do
          runStubTest (status "Personal" SQMessages)
          `shouldBe` Just 2
      describe "recent item" $ do
        it "status on 'INBOX' should return with two mails" $ do
          runStubTest (status "." SQRecent)
          `shouldBe` Just 2
        it "status on 'Personal' should return with two mails" $ do
          runStubTest (status "Personal" SQRecent)
          `shouldBe` Just 2
      describe "uidnext item" $ do
        it "status on 'INBOX' should return with two mails" $ do
          runStubTest (status "." SQUidnext)
          `shouldBe` Just undefined
        it "status on 'Personal' should return with two mails" $ do
          runStubTest (status "Personal" SQUidnext)
          `shouldBe` Just undefined
      describe "uidvalidity item" $ do
        it "status on 'INBOX' should return with two mails" $ do
          runStubTest (status "." SQUidvalidity)
          `shouldBe` Just undefined
        it "status on 'Personal' should return with two mails" $ do
          runStubTest (status "Personal" SQUidvalidity)
          `shouldBe` Just undefined
      describe "unseen and messages item" $ do
        it "status on 'INBOX' should return with two mails" $ do
          runStubTest (status "." (SQProduct SQUnseen SQMessages))
          `shouldBe` Just (2, 2)
        it "status on 'Personal' should return with two mails" $ do
          runStubTest (status "Personal" (SQProduct SQUnseen SQMessages))
          `shouldBe` Just (2, 2)
      describe "all items" $ do
        it "status on 'INBOX' should return with no mail" $ do
          runStubTest (status "." (SQProduct (SQProduct SQUnseen SQMessages) (SQProduct SQRecent (SQProduct SQUidnext SQUidvalidity))))
          `shouldBe` Just undefined
        it "status on 'Personal' should return with no mail" $ do
          runStubTest (status "Personal" (SQProduct (SQProduct SQUnseen SQMessages) (SQProduct SQRecent (SQProduct SQUidnext SQUidvalidity))))
          `shouldBe` Just undefined
