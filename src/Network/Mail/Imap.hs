{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap
  ( Imap
  , searchAll
  , fetchHeader
  ) where

import Network.Mail
import Network.Mail.Imap.Types
import Control.Monad.Free (liftF)

searchAll :: Imap (Maybe [UID])
searchAll = liftF $ Search undefined id

fetchHeader :: [UID] -> Imap (Maybe [Header])
fetchHeader uids = liftF $ Fetch uids FQHeader id
