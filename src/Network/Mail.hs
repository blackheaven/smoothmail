module Network.Mail
  ( Mail(..)
  , UID(..)
  , Header(..)
  ) where

data Mail = Mail { getHeader :: Header } deriving (Show, Ord, Eq) -- Body + Header
data Header = Header {
              getUID     :: UID
            , getDate    :: String
            , getSender  :: String
            , getSubject :: String
            } deriving (Show, Ord, Eq) -- Envelope
newtype UID = UID { extractUID :: Integer } deriving (Show, Ord, Eq)

