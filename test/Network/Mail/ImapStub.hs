{-# LANGUAGE GADTs #-}

module Network.Mail.ImapStub (runStubTest, debugStubTest) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.Imap.Types
import Control.Monad.Free (iterM)
import Control.Monad.State
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intercalate)
import Data.Function (on)
import qualified Data.Map as M

mails = M.fromList  [ ("/", M.fromList [ -- aka root or INBOX
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
           Search _            n -> onSearch n
           Fetch uids FQHeader n -> onFetch uids n
           Select p            n -> onSelect p n
           Create d            n -> onCreate d n
           Rename d            n -> onRename d n
           Delete d            n -> onDelete d n
           Subscribe d         n -> onSubscribe d n
           Unsubscribe d       n -> onUnsubscribe d n
           List (Left d)       n -> onList d n
           Lsub (Left d)       n -> onLsub d n
           Expunge             n -> onExpunge n
           Check               n -> n
           Examine p           n -> onExamine p n
           Noop                n -> onNoop n

onSearch              n = get >>= \d -> n $ fmap (M.keys) (M.lookup d mails)
onFetch uids          n = get >>= \d -> n $ fmap (\m -> map getHeader $ mapMaybe (flip M.lookup m) uids) (M.lookup d mails)
onSelect p            n = get >>= \o -> put (canonicalize $ o ++ "/" ++ p) >> get >>= \d -> n (fmap makeDirectoryDescription (M.lookup d mails))
onCreate d            n = get >>= \o -> n . not $ isExistingDirectory (o ++ "/" ++ d)
onRename d            n = get >>= \o -> n $ (isExistingDirectory o) && (not $ isExistingDirectory (o ++ "/../" ++ d))
onDelete d            n = get >>= \o -> n $ isExistingDirectory (o ++ "/" ++ d)
onSubscribe d         n = get >>= \o -> n $ isExistingDirectory (o ++ "/" ++ d)
onUnsubscribe d       n = get >>= \o -> n $ isExistingDirectory (o ++ "/" ++ d)
onList d              n = get >>= \o -> n $ Just $ getSubdirectories (canonicalize (o ++ "/" ++ d))
onLsub d              n = get >>= \o -> n $ Just $ getSubdirectories (canonicalize (o ++ "/" ++ d))
onExpunge             n = get >>= \o -> n $ o == "/" -- We only allow to delete old messages if we are on the root folder
onExamine p           n = get >>= \o -> n $ fmap makeDirectoryDescription (M.lookup (canonicalize $ o ++ "/" ++ p) mails)
onNoop                n = get >>= \o -> n $ fromMaybe (DirectoryDescription 0 2 0) $ fmap makeDirectoryDescription  (M.lookup o mails)

canonicalize p = onNull "/" $ case p of
                                ('/':'/':r) -> canonicalize ('/':r)
                                _           -> flattenLevels p
flattenLevels = intercalate "/" . dropUp . splitOn '/'
splitOn s a = case a of
               [] -> []
               _  -> (\(p, n) -> p:splitOn s (drop 1 n)) (break (== s) a)
dropUp s = case s of
            []          -> []
            (_:"..":xs) -> dropUp xs
            (".":xs)  -> dropUp xs
            (x:xs)      -> x:dropUp xs
onNull c r = if null r then c else r
isExistingDirectory d = elem (canonicalize d) (M.keys mails)
getSubdirectories root = let rs = splitOn '/' root in map last $ filter (and . zipWith (==) rs) $ filter (\ds -> ((length rs) + 1) == (length ds))  $ map (splitOn '/') $ map fst $ M.toList mails
makeDirectoryDescription d = DirectoryDescription 0 (toInteger $ M.size d) 0
