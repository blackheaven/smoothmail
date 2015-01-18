{-# LANGUAGE GADTs #-}

module Network.Mail.ImapStub (runStubTest) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.Imap.Types
import Control.Monad.Free (iter)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M


mails = M.fromList $ [
                        (UID 1, Mail (Header (UID 1) "2015-01-01 10:10" "S1" "T1"))
                     ,  (UID 2, Mail (Header (UID 2) "2015-02-03 21:12" "S2" "T2"))
                     ]

runStubTest :: Imap a -> a
runStubTest = iter $ \x -> case x of
                             Search     _ n        -> n $ Just $ M.keys mails
                             Fetch uids FQHeader n -> n $ Just $ map getHeader $ mapMaybe (flip M.lookup mails) uids

