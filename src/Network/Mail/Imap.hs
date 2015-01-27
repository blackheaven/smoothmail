{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap
  ( Imap
  , searchAll
  , fetchHeader
  , select
  , create
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
