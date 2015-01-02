module Network.Mail
  ( Mail
  , UID
  , Header
  ) where

data Mail = Mail { getHeader :: Header }
data Header = Header { getUID :: UID }
newtype UID = UID { extractUID :: Integer }
