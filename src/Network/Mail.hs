module Network.Mail
  ( Mail
  , UID(..)
  , Header
  ) where

data Mail = Mail { getHeader :: Header } -- Body + Header
data Header = Header { getUID :: UID } -- Envelope
newtype UID = UID { extractUID :: Integer } deriving (Show, Ord, Eq)
