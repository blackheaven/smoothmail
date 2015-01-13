{-# LANGUAGE FlexibleInstances #-}

module Network.Mail.ImapStub (runStubTest) where

import Test.Hspec
import Network.Mail.Imap

class StubRunner a where
    runStubTest :: Imap a -> a

instance StubRunner [String] where
    runStubTest _ =  [
                     "1 2015-01-01 10:10 S1 T1"
                     , "2 2015-02-03 21:12 S1 T2"
                     ]
