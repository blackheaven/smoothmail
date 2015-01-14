{-# LANGUAGE FlexibleInstances #-}

module Network.Mail.ImapStub (runStubTest) where

import Test.Hspec
import Network.Mail.Imap
import Network.Mail.Imap.Types
import Control.Monad.Free (iter)

mails = [
          "1 2015-01-01 10:10 S1 T1"
        , "2 2015-02-03 21:12 S1 T2"
        ]

runStubTest :: Imap a -> a
runStubTest = iter $ \x -> case x of
                             Search _ _ -> undefined
