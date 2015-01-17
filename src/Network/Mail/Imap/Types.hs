{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.Mail.Imap.Types
  ( Imap
  , ImapF(..)
  , DirectoryName
  , DirectoryDescription
  , DirectoryPattern
  , DirectorySearch
  , StatusDataItemName
  , MailSearch
  , Flag
  , Flags
  , FlagUpdate
  , FetchQuery(..)
  , Size
  ) where

import Network.Mail
import Control.Monad.Free

data ImapF next =
      Select DirectoryName                      (Maybe DirectoryDescription -> next)
    | Examine DirectoryName                     (Maybe DirectoryDescription -> next)
    | Noop                                      (DirectoryDescription -> next)
    | Create DirectoryName                      (Bool -> next)
    | Delete DirectoryName                      (Bool -> next)
    | Rename DirectoryName                      (Bool -> next)
    | Subscribe DirectoryName                   (Bool -> next)
    | Unsubscribe DirectoryName                 (Bool -> next)
    | List DirectorySearch                      (Maybe [DirectoryName] -> next)
    | Lsub DirectorySearch                      (Maybe [DirectoryName] -> next)
    | Status DirectoryName [StatusDataItemName] (Maybe DirectoryDescription -> next)
    | Append (Maybe Flags) (Maybe Date) Mail    (Bool -> next)
    | Check                                     next -- Selected state only
    | Expunge                                   (Bool -> next) -- S
    | Search MailSearch                         (Maybe [UID] -> next) -- S
    | forall a. Fetch [UID] (FetchQuery a)      (Maybe [a] -> next) -- S
    | Store [UID] FlagUpdate                    (Bool -> next) -- S
    | Copy [UID] DirectoryName                  (Bool -> next) -- S
    -- UID -- S TODO

instance Functor ImapF where
    fmap f v = case v of
                Select a n      -> Select a      (f . n)
                Examine a n     -> Examine a     (f . n)
                Noop n          -> Noop          (f . n)
                Create a n      -> Create a      (f . n)
                Delete a n      -> Delete a      (f . n)
                Rename a n      -> Rename a      (f . n)
                Subscribe a n   -> Subscribe a   (f . n)
                Unsubscribe a n -> Unsubscribe a (f . n)
                List a n        -> List a        (f . n)
                Lsub a n        -> Lsub a        (f . n)
                Status a b n    -> Status a b    (f . n)
                Append a b c n  -> Append a b c  (f . n)
                Check n         -> Check         (f n)
                Expunge n       -> Expunge       (f . n)
                Search a n      -> Search a      (f . n)
                Fetch a b n     -> Fetch a b     (f . n)
                Store a b n     -> Store a b     (f . n)
                Copy a b n      -> Copy a b      (f . n)

type Imap = Free ImapF

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
