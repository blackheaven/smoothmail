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

searchAll :: Imap [UID]
searchAll = undefined

fetchAll :: [UID] -> Imap [Mail]
fetchAll _ = undefined

getUID :: Mail -> String
getUID _ = undefined

getDate :: Mail -> String
getDate _ = undefined

getSender :: Mail -> String
getSender _ = undefined

getSubject :: Mail -> String
getSubject _ = undefined

