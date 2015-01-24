{-# LANGUAGE GADTs #-}

module Network.Mail.ImapStub (runStubTest) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.Imap.Types
import Control.Monad.Free (iterM)
import Control.Monad.State
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

mails = M.fromList  [ ("", M.fromList [
                                       (UID 1, Mail (Header (UID 1) "2015-01-01 10:10" "S1" "T1"))
                                      , (UID 2, Mail (Header (UID 2) "2015-02-03 21:12" "S2" "T2"))
                                      ])
                    , ("Personal", M.fromList [(UID 3, Mail (Header (UID 3) "2015-04-05 12:34" "S2" "T1"))])
                    ]

runStubTest :: Imap a -> a
runStubTest = flip evalState "" . iterM eval
  where eval :: ImapF (State String a) -> State String a
        eval x = case x of
                   Search _            n -> get >>= \d -> n $ fmap (M.keys) (M.lookup d mails)
                   Fetch uids FQHeader n -> get >>= \d -> n $ fmap (\m -> map getHeader $ mapMaybe (flip M.lookup m) uids) (M.lookup d mails)
                   Select p            n -> get >>= \o -> put (reduce $ o ++ "/" ++ p) >> get >>= \d -> n (fmap (const undefined) (M.lookup d mails))
        reduce p = case p of
                     "/" -> ""
                     _ -> p
