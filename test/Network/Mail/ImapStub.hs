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

mails = M.fromList $ [
                        (UID 1, Mail (Header (UID 1) "2015-01-01 10:10" "S1" "T1"))
                     ,  (UID 2, Mail (Header (UID 2) "2015-02-03 21:12" "S2" "T2"))
                     ]

mailsPersonal = M.fromList [(UID 3, Mail (Header (UID 3) "2015-04-05 12:34" "S2" "T1"))]

runStubTest :: Imap a -> a
runStubTest = flip evalState mails . iterM eval
  where eval :: ImapF (State (M.Map UID Mail) a) -> State (M.Map UID Mail) a
        eval x = case x of
                   Search _            n -> get >>= n . Just . M.keys
                   Fetch uids FQHeader n -> get >>= \m -> n $ Just $ map getHeader $ mapMaybe (flip M.lookup m) uids
                   Select _            n -> put mailsPersonal >> n (Just undefined)
