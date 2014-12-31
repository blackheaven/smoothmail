module Data.Network.Mail
  ( Mail
  , UID
  ) where

data Mail = Mail { getID :: UID }
newtype UID = UID { getUID :: Integer }
