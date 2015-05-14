{-# LANGUAGE GADTs #-}

module Network.Mail.ImapStub (runStubTest, debugStubTest) where

import Test.Hspec
import Network.Mail
import Network.Mail.Imap
import Network.Mail.Imap.Types
import Control.Monad.Free (iterM)
import Control.Monad.State
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Data.List (intercalate)
import Data.Function (on)
import qualified Data.Map as M

mails :: M.Map String (M.Map UID Mail)
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
           Status d i          n -> onStatus d i n

-- actions
onSearch :: (Maybe [UID] -> State String a) -> State String a
onSearch n = do
  currentDirectory <- get
  n $ fmap (M.keys) (M.lookup currentDirectory mails)

onFetch :: [UID] -> (Maybe [Header] -> State String b) -> State String b
onFetch uids n = do
  currentDirectory <- get
  n $ fmap (\m -> map getHeader $ mapMaybe (flip M.lookup m) uids) (M.lookup currentDirectory mails)

onSelect :: String -> (Maybe DirectoryDescription -> State String b) -> State String b
onSelect p n = do
  currentDirectory <- get
  let newDirectory = canonicalize $ currentDirectory ++ "/" ++ p
  let newDirectoryStatus = M.lookup newDirectory mails
  if isJust newDirectoryStatus then put newDirectory else return ()
  n $ fmap makeDirectoryDescription newDirectoryStatus

onCreate :: String -> (Bool -> State String b) -> State String b
onCreate d n = do
  currentDirectory <- get
  n . not $ isExistingDirectory (currentDirectory ++ "/" ++ d)

onRename :: String -> (Bool -> State String b) -> State String b
onRename d n = do
  currentDirectory <- get
  n $ (isExistingDirectory currentDirectory) && (not $ isExistingDirectory (currentDirectory ++ "/../" ++ d))

onDelete :: String -> (Bool -> State String b) -> State String b
onDelete d n = do
  currentDirectory <- get
  n $ isExistingDirectory (currentDirectory ++ "/" ++ d)

onSubscribe :: String -> (Bool -> State String b) -> State String b
onSubscribe d n = do
  currentDirectory <- get
  n $ isExistingDirectory (currentDirectory ++ "/" ++ d)

onUnsubscribe :: String -> (Bool -> State String b) -> State String b
onUnsubscribe d n = do
  currentDirectory <- get
  n $ isExistingDirectory (currentDirectory ++ "/" ++ d)

onList :: String -> (Maybe [String] -> State String b) -> State String b
onList d n = do
  currentDirectory <- get
  n $ Just $ getSubdirectories (canonicalize (currentDirectory ++ "/" ++ d))

onLsub :: String -> (Maybe [String] -> State String b) -> State String b
onLsub d n = do
  currentDirectory <- get
  n $ Just $ getSubdirectories (canonicalize (currentDirectory ++ "/" ++ d))

onExpunge :: (Bool -> State String b) -> State String b
onExpunge n = do
  currentDirectory <- get
  n $ currentDirectory == "/" -- We only allow to delete old messages if we are on the root folder

onExamine :: String -> (Maybe DirectoryDescription -> State String b) -> State String b
onExamine p n = do
  currentDirectory <- get
  n $ fmap makeDirectoryDescription (M.lookup (canonicalize $ currentDirectory ++ "/" ++ p) mails)

onNoop :: (DirectoryDescription -> State String b) -> State String b
onNoop n = do
  currentDirectory <- get
  n $ fromMaybe (DirectoryDescription 0 2 0) $ fmap makeDirectoryDescription  (M.lookup currentDirectory mails)

onStatus :: DirectoryName -> (StatusQuery a) -> (Maybe a -> State String b) -> State String b
onStatus d i n = do
  currentDirectory <- get
  let targettedDirectory = canonicalize $ currentDirectory ++ "/" ++ d
  n $ fmap (extractInfo i) (M.lookup targettedDirectory mails)
  where extractInfo :: StatusQuery a -> M.Map UID Mail -> a
        extractInfo q m = case q of
                          SQProduct a b -> (extractInfo a m, extractInfo b m)
                          SQMessages    -> 2
                          SQRecent      -> 2
                          SQUidnext     -> UID $ (1+) . extractUID $ maximum $ map fst $ concatMap (M.toList . snd) $ M.toList mails
                          SQUidvalidity -> UID $ extractUID $ maximum $ map fst $ M.toList m
                          SQUnseen      -> 2

-- Helpers
canonicalize :: String -> String
canonicalize p = onNull "/" $ case p of
                                ('/':'/':r) -> canonicalize ('/':r)
                                _           -> flattenLevels p

flattenLevels :: String -> String
flattenLevels = intercalate "/" . dropUp . splitOn '/'

splitOn :: Eq t => t -> [t] -> [[t]]
splitOn s a = case a of
               [] -> []
               _  -> (\(p, n) -> p:splitOn s (drop 1 n)) (break (== s) a)

dropUp :: [String] -> [String]
dropUp s = case s of
            []          -> []
            (_:"..":xs) -> dropUp xs
            (".":xs)    -> dropUp xs
            (x:xs)      -> x:dropUp xs

onNull :: [a] -> [a] -> [a]
onNull c r = if null r then c else r

isExistingDirectory :: String -> Bool
isExistingDirectory d = elem (canonicalize d) (M.keys mails)

getSubdirectories :: String -> [String]
getSubdirectories root = let rs = splitOn '/' root in map last $ filter (and . zipWith (==) rs) $ filter (\ds -> ((length rs) + 1) == (length ds))  $ map (splitOn '/') $ map fst $ M.toList mails

makeDirectoryDescription :: M.Map k a -> DirectoryDescription
makeDirectoryDescription d = DirectoryDescription 0 (toInteger $ M.size d) 0

