module Data.Network.Imap
  ( Imap
  ) where

import Data.Network.Mail

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

data Imap :: * -> * where
    Select :: UID -> Imap Mail

