{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap
  ( Imap
  , searchAll
  , fetchAll
  ) where

import Network.Mail
import Network.Mail.Imap.Types
import Control.Monad.Free (liftF)

searchAll :: Imap [UID]
searchAll = liftF $ Search undefined undefined

fetchAll :: [UID] -> Imap [Mail]
fetchAll uids = liftF $ Fetch uids undefined undefined
