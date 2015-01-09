{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap.Types
  ( Imap
  , DirectoryName
  , DirectoryDescription
  , DirectoryPattern
  , DirectorySearch
  , StatusDataItemName
  , MailSearch
  , Flag
  , Flags
  , FlagUpdate
  , FetchQuery
  , Size
  ) where

import Network.Mail
import Control.Applicative

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
    -- UID -- S TODO

instance Functor Imap where
    fmap f v = case v of
                 _ -> undefined

instance Applicative Imap where
    pure _ = undefined
    _ <*> _ = undefined

instance Monad Imap where
  return _ = undefined
  _ >>= _ = undefined

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

data MailSearch = MSAll
                | MSAnswered
                | MSBcc String
                | MSBefore Date
                | MSBody String
                | MSCc String
                | MSDeleted
                | MSDraft
                | MSFlagged
                | MSFrom String
                | MSHeader String String
                | MSKeyword Flag
                | MSLarger Size
                | MSNew
                | MSNot MailSearch
                | MSOld
                | MSOn Date
                | MSOr MailSearch MailSearch
                | MSRecent
                | MSSeen
                | MSSentbefore Date
                | MSSenton Date
                | MSSentsince Date
                | MSSince Date
                | MSSmaller Size
                | MSSubject String
                | MSText String
                | MSTo String
                | MSUid [UID]
                | MSUnanswered
                | MSUndeleted
                | MSUndraft
                | MSUnflagged
                | MSUnkeyword Flag
                | MSUnseen

data Flag = Flag -- TODO

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

type Size = Integer
data Date = Data -- TODO
