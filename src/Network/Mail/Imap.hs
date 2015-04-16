{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap
  ( Imap
  , searchAll
  , fetchHeader
  , select
  , create
  , rename
  , delete
  , subscribe
  , unsubscribe
  , list
  , lsub
  , expunge
  , check
  , examine
  , noop
  , status
  ) where

import Network.Mail
import Network.Mail.Imap.Types
import Control.Monad.Free (liftF)

searchAll :: Imap (Maybe [UID])
searchAll = liftF $ Search undefined id

fetchHeader :: [UID] -> Imap (Maybe [Header])
fetchHeader uids = liftF $ Fetch uids FQHeader id

select :: DirectoryName -> Imap (Maybe DirectoryDescription)
select directory = liftF $ Select directory id

create :: DirectoryName -> Imap Bool
create directory = liftF $ Create directory id

rename :: DirectoryName -> Imap Bool
rename directory = liftF $ Rename directory id

delete :: DirectoryName -> Imap Bool
delete directory = liftF $ Delete directory id

subscribe :: DirectoryName -> Imap Bool
subscribe directory = liftF $ Subscribe directory id

unsubscribe :: DirectoryName -> Imap Bool
unsubscribe directory = liftF $ Unsubscribe directory id

list :: DirectorySearch -> Imap (Maybe [DirectoryName])
list directorySearch = liftF $ List directorySearch id

lsub :: DirectorySearch -> Imap (Maybe [DirectoryName])
lsub directorySearch = liftF $ Lsub directorySearch id

expunge :: Imap Bool
expunge = liftF $ Expunge id

check :: Imap ()
check = liftF $ Check ()

examine :: DirectoryName -> Imap (Maybe DirectoryDescription)
examine directory = liftF $ Examine directory id

noop :: Imap DirectoryDescription
noop = liftF $ Noop id

status :: DirectoryName -> (StatusQuery a) -> Imap (Maybe a)
status n i = liftF $ Status n i id

-- TODO
-- # All State
-- * capability
-- * logout
-- # Not Authenticated State
-- * starttls
-- * authenticate
-- * login
-- # Selected State
-- * close
