{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Mail.Imap
  ( Imap
  ) where

import Network.Mail

data Imap :: * -> * where
    Select :: UID -> Imap Mail

