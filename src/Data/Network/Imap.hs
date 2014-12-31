{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Network.Imap
  ( Imap
  ) where

import Data.Network.Mail

data Imap :: * -> * where
    Select :: UID -> Imap Mail

