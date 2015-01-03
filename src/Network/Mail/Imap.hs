{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap
  ( Imap
  ) where

import Network.Mail

data Imap :: * -> * where
    Select :: DirectoryName -> Imap (Maybe DirectoryDescription)
    Examine :: DirectoryName -> Imap (Maybe DirectoryDescription)
    Noop :: Imap DirectoryDescription
    Create :: DirectoryName -> Imap Bool
    Delete :: DirectoryName -> Imap Bool
    Rename :: DirectoryName -> Imap Bool
    Subscribe :: DirectoryName -> Imap Bool
    Unsubscribe :: DirectoryName -> Imap Bool
    List :: DirectorySearch -> Imap (Maybe [DirectoryName])
    Lsub :: DirectorySearch -> Imap (Maybe [DirectoryName])
    Status :: DirectoryName -> [StatusDataItemName] -> Imap (Maybe DirectoryDescription)
    Append :: Maybe Flags -> Maybe Date -> Mail -> Imap Bool
    Check :: Imap () -- Selected state only
    Expunge :: Imap Bool -- S
    Search :: MailSearch -> Imap (Maybe [UID]) -- S
    Fetch :: [UID] -> FetchQuery a -> Imap (Maybe [a]) -- S
    Store :: [UID] -> FlagUpdate -> Imap Bool -- S
    Copy :: [UID] -> DirectoryName -> Imap Bool -- S


type DirectoryName = String
data DirectoryDescription = DirectoryDescription
                          { expunge :: Integer
                          , exists :: Integer
                          , recent :: Integer
                          }

type DirectoryPattern = String
type DirectorySearch = Either DirectoryName DirectoryPattern

data StatusDataItemName = Messages
                        | Recent
                        | Uidnext
                        | Uidvalidity
                        | Unseen

data MailSearch = All
                | Answered
                | Bcc String
                | Before Date
                | Body String
                | Cc String
                | Deleted
                | Draft
                | Flagged
                | From String
                | Header String String
                | Keyword Flag
                | Larger Size
                | New
                | Not MailSearch
                | Old
                | On Date
                | Or MailSearch MailSearch
                | Recent
                | Seen
                | Sentbefore Date
                | Senton Date
                | Sentsince Date
                | Since Date
                | Smaller Size
                | Subject String
                | Text String
                | To String
                | Uid [UID]
                | Unanswered
                | Undeleted
                | Undraft
                | Unflagged
                | Unkeyword Flag
                | Unseen

data Flag

type Flags = [Flag]

data FlagUpdate = FlagSet Flags
                | FlagAdd Flags
                | FlagDelete Flags
                | FlagUpdateProduct FlagUpdate FlagUpdate

data FetchQuery :: * -> * where
    -- Macros
    FQAll :: FetchQuery (Flags, Date, Size, Header)
    FQFast :: FetchQuery (Flags, Date, Size)
    FQFull :: FetchQuery (Flags, Date, Size, Header, Mail)
    -- Composition
    FQProduct :: FetchQuery a -> FetchQuery b -> FetchQuery (a, b)
    -- Units
    FQFlags  :: FetchQuery Flags
    FQDate   :: FetchQuery Date
    FQSize   :: FetchQuery Size
    FQHeader :: FetchQuery Header
    FQMail   :: FetchQuery Mail

