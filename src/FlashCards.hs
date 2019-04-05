{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FlashCards where 


import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import Data.Foldable
import GHC.Generics
import Data.Aeson
import Control.Monad.IO.Class 

data Vocabulary = Vocabulary { author  :: T.Text,
                               title   :: T.Text,
                               time    :: Integer,
                               wordKey :: T.Text,
                               usage   :: T.Text
                             } deriving (Eq, Show, Generic) 
                               
instance ToJSON Vocabulary
instance FromJSON Vocabulary


vocabs :: IO ()
vocabs = do 
   conn <- open "vocab.db"
   vcs <- connectionHandler conn
   close conn
   print vcs 

   
  
connectionHandler :: Connection ->  IO  [Vocabulary]
connectionHandler conn  = do
  wordLookups  <-  query_ conn "select word_key, book_key, usage, timestamp from lookups" :: IO [(String, String, String, Integer)]
  traverse (bookDetail conn)  wordLookups
   
  where
    bookDetail :: Connection -> (String, String, String, Integer) -> IO Vocabulary
    bookDetail c (w, key, u, t)  =  do 
      detail@ (x : xs)  <- queryNamed c "SELECT title, authors FROM book_info WHERE guid=:book_key" [":book_key" := key]
      return $ Vocabulary (snd x) (fst x) t (T.pack w) (T.pack u)
  
