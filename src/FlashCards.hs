{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FlashCards where 


import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import Data.Foldable
import GHC.Generics
import Data.Aeson
import Control.Monad.IO.Class
import Data.Typeable

data Vocabulary = Vocabulary { author     :: T.Text,
                               title      :: T.Text,
                               timeS       :: Integer,
                               wordKey    :: T.Text,
                               usage      :: T.Text,
                               mastered   :: Bool,
                               deleted    :: Bool,
                               bookKey    :: T.Text 
                             } deriving (Eq, Show, Generic) 

intToBool :: Integer -> Bool
intToBool i
  | i == 0 = False
  | i == 1 = True
  | otherwise = False

boolToInt :: Bool -> Integer
boolToInt b
  | b == False = 0
  | b == True = 1
  | otherwise = 0

instance ToJSON Vocabulary
instance FromJSON Vocabulary


vocabs :: IO ()
vocabs = do 
   conn <- open "vocab.db"
   vcs <- connectionHandler conn
   close conn
   print vcs 


newtype DebugShowType = DebugShowType String deriving (Eq, Show, Typeable)

instance FromField DebugShowType where
  fromField f = cvt f . fieldData $ f where
    cvt _ v = Ok $ DebugShowType (show v)

  
connectionHandler :: Connection ->  IO  [Vocabulary]
connectionHandler conn  = do
  columns <- query_ conn "PRAGMA table_info (LOOKUPS)" :: IO [[DebugShowType]]
  liftIO $ insertNewColumn conn (length columns)
  wordLookups  <-  query_ conn "select word_key, book_key, usage, timestamp, isMastered, isDeleted  from lookups" :: IO [(String, String, String, Integer, Integer, Integer)]
  traverse (bookDetail conn) wordLookups
  

   
  where
    bookDetail :: Connection -> (String, String, String, Integer, Integer, Integer) -> IO Vocabulary
    bookDetail c (w, key, u, t, iM, iD)  =  do
      detail@ (x : xs)  <- queryNamed c "SELECT title, authors FROM book_info WHERE guid=:book_key" [":book_key" := key]
      return $ Vocabulary (snd x) (fst x) t (T.pack w) (T.pack u) (intToBool iM) (intToBool iD) (T.pack key)

    insertNewColumn :: Connection -> Int -> IO ()
    insertNewColumn c i
        | i == 7 = do
            (execute_ c "ALTER TABLE LOOKUPS ADD isDeleted INTEGER default 0")
            (execute_ c "ALTER TABLE LOOKUPS ADD isMastered INTEGER default 0")
            return () 
        | otherwise = return ()
        

updateInDatabase :: Connection -> Bool -> Bool -> Integer ->  IO ()
updateInDatabase  conn m d t = do
    executeNamed conn "UPDATE lookups SET isMastered = :iM, isDeleted = :iD  WHERE timestamp = :tm" [":iM" := boolToInt m, ":iD" := boolToInt d, ":tm" := t]
  -- executeNamed conn "UPDATE lookups SET  isMastered = :iD  WHERE timestamp = :tm" [":iD" := (boolToInt m :: Integer), ":tm" := t]
  
  
   
  


vocabDel :: Connection -> Integer -> IO ()
vocabDel c i = do
  liftIO $ print i 
  executeNamed c "UPDATE lookups SET  isDeleted = :iD  WHERE timestamp = :tm" [":iD" := (1 :: Integer), ":tm" := i]
 
  
  
  
-- | create Data Base
initDB ::  IO ()
initDB  = withConnection "myvocabulary.db" $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS myVocabulary ( authorD text not null,     titleD text not null,    timD integer not null,     wordKeyD text not null, usaageD text not null,     masteredD bool not null,     deletedD bool not null, bookkeyD text not null)"
