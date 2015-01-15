{-# LANGUAGE FlexibleInstances #-}

module Network.Mail.ImapStub (runStubTest) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.Imap.Types
import Control.Monad.Free (iter)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M


mails = M.fromList $ [
                        (UID 1, undefined)
                     ,  (UID 2, undefined)
                     ]

runStubTest :: Imap a -> a
runStubTest = iter $ \x -> case x of
                             Search     _ n -> n $ Just $ M.keys mails
                             Fetch uids _ n -> n $ Just $ mapMaybe (flip M.lookup mails) uids

