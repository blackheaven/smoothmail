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

searchAll :: Imap [UID]
searchAll = liftF $ Search undefined undefined

fetchHeader :: [UID] -> Imap [Header]
fetchHeader uids = liftF $ Fetch uids FQHeader undefined
