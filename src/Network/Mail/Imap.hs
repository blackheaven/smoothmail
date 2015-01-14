{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap
  ( Imap
  , searchAll
  , fetchAll
  , getUID
  , getDate
  , getSender
  , getSubject
  ) where

import Network.Mail
import Network.Mail.Imap.Types
import Control.Monad.Free (liftF)

searchAll :: Imap [UID]
searchAll = liftF $ Search undefined undefined

fetchAll :: [UID] -> Imap [Mail]
fetchAll uids = liftF $ Fetch uids undefined undefined

getUID :: Mail -> String
getUID _ = undefined

getDate :: Mail -> String
getDate _ = undefined

getSender :: Mail -> String
getSender _ = undefined

getSubject :: Mail -> String
getSubject _ = undefined

