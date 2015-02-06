{-# LANGUAGE GADTs #-}

module Network.Mail.ImapStub (runStubTest, debugStubTest) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.Imap.Types
import Control.Monad.Free (iterM)
import Control.Monad.State
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import qualified Data.Map as M

mails = M.fromList  [ ("/", M.fromList [
                                       (UID 1, Mail (Header (UID 1) "2015-01-01 10:10" "S1" "T1"))
                                      , (UID 2, Mail (Header (UID 2) "2015-02-03 21:12" "S2" "T2"))
                                      ])
                    , ("/Personal", M.fromList [(UID 3, Mail (Header (UID 3) "2015-04-05 12:34" "S2" "T1"))])
                    , ("/Work", M.fromList [])
                    ]

runStubTest :: Imap a -> a
runStubTest = flip evalState "/" . iterM eval

debugStubTest :: Imap a -> (a, String)
debugStubTest = flip runState "/" . iterM eval

eval :: ImapF (State String a) -> State String a
eval x = case x of
           Search _            n -> get >>= \d -> n $ fmap (M.keys) (M.lookup d mails)
           Fetch uids FQHeader n -> get >>= \d -> n $ fmap (\m -> map getHeader $ mapMaybe (flip M.lookup m) uids) (M.lookup d mails)
           Select p            n -> get >>= \o -> put (canonicalize $ o ++ "/" ++ p) >> get >>= \d -> n (fmap (const undefined) (M.lookup d mails))
           Create d            n -> get >>= \o -> n . not $ isExistingDirectory (o ++ "/" ++ d)
           Rename d            n -> get >>= \o -> n $ (isExistingDirectory o) && (not $ isExistingDirectory (o ++ "/../" ++ d))
           Delete d            n -> get >>= \o -> n $ isExistingDirectory (o ++ "/" ++ d)
           Subscribe d         n -> get >>= \o -> n $ isExistingDirectory (o ++ "/" ++ d)
  where canonicalize p = onNull "/" $ case p of
                                        ('/':'/':r) -> canonicalize ('/':r)
                                        _           -> flattenLevels p
        flattenLevels = intercalate "/" . dropUp . splitOn '/'
        splitOn s a = case a of
                       [] -> []
                       _  -> (\(p, n) -> p:splitOn s (drop 1 n)) (break (== s) a)
        dropUp s = case s of
                    []          -> []
                    (_:"..":xs) -> dropUp xs
                    (x:xs)      -> x:dropUp xs
        onNull c r = if null r then c else r
        isExistingDirectory d = elem (canonicalize d) (M.keys mails)
