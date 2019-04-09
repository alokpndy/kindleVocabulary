
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module ClipApi  where

import Network.Wai.Handler.Warp (run)
import Data.Aeson 
import Servant
import GHC.Generics
import Servant.Swagger
import Servant.Swagger.UI
import Data.Swagger
import GHC.TypeLits
import Data.Text (Text)
import Data.List
import qualified Data.Text.IO   as T
import Filter 
import Types
import FlashCards
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import Control.Monad.IO.Class
import Servant.HTML.Blaze
import Text.Blaze.Html5 hiding (main)





-- | Clippings ----------------------------------------------
clips2 :: Text ->  [Clipping]
clips2 file = do
      onlHighlight file
      
data SortBy = Ascending | Descending  deriving (Generic) 
data ListBy  = Author SortBy | Book SortBy deriving (Generic) 

instance ToParamSchema SortBy

instance  FromHttpApiData ListBy where
  parseQueryParam param = do
     s  <- parseUrlPiece param :: Either Text ListBy 
     case s of
       _  -> return $ Author Ascending -- temp
       
instance  FromHttpApiData SortBy where
  parseQueryParam param = do
     s  <- parseUrlPiece param :: Either Text Text 
     case s of
       "Ascending"  -> return  Ascending -- temp
       "Descending" -> return  Descending
       _            -> Left $ "Unspecifed Sort Order "




-- | Endpoints ----------------------------------------------- 
type ClipApi =
       "clippingGetAll" :> QueryParam "listBy" SortBy :> QueryParams "getBy" String
       :> QueryFlag "fav" :> Get '[JSON] [Clipping]
type VocApi = "vocabGetAll" :> Get '[JSON] [Vocabulary]
type VocQueryApi = "vocabGetBy" :> QueryFlag "isMastered" :> QueryFlag "isDeleted"
              :> Capture "getListOf" Int  :> QueryParam "fromTimestamp" Integer :> Get '[JSON] [Vocabulary]
type VocUpdateApi = "vocUpdate" :> ReqBody '[JSON] [Vocabulary] :> PutNoContent '[JSON] NoContent
type VocabDeleteApi = "vocDelete" :> Capture "timestamp" Integer :> DeleteNoContent '[JSON] NoContent
      
type Api = ClipApi :<|> VocApi :<|> VocQueryApi :<|> VocUpdateApi :<|> VocabDeleteApi 



-- | Server --------------------------------------------------      
server :: Text  -> Connection ->  Server Api
server tx c  = do
  clippingGetAll :<|> vocabGetAll :<|> vocQuery :<|> vocUpdate :<|> vocabDelete  
  
  where
    vocabGetAll ::   Handler [Vocabulary]
    vocabGetAll = 
      liftIO $ do 
         con <- open "vocab.db"
         connectionHandler con <* close con 
     
      

    vocQuery :: Bool -> Bool -> Int -> Maybe  Integer -> Handler [Vocabulary]
    vocQuery m1 d1 l1 td = do
      liftIO  $  do
        con <- open "vocab.db"
        all <-  queryNumbersOf con l1 td
        close con 
        return $ take l1 $  filter (\(Vocabulary a tt t wk u m d bk) -> m == m1 && d1 == d) all

    vocUpdate ::  [Vocabulary] -> Handler NoContent
    vocUpdate vo = 
      liftIO $ do
         con <- open "vocab.db"
         traverse (\v ->   updateInDatabase con  (mastered v) (deleted v) (timeS v)) vo
         close con
         return NoContent 
    
    vocabDelete :: Integer -> Handler NoContent
    vocabDelete i = 
      liftIO $ do
        con <- open "vocab.db"
        vocabDel con i
        close con
        return NoContent
    
    clippingGetAll :: Maybe SortBy -> [String] ->  Bool -> Handler [Clipping]
    clippingGetAll qp xs qf = case qp of
      Nothing -> return []
      Just x -> case x of
        Ascending -> if qf == True then return  (clips2 tx)
                            else return (clips2 tx)
        Descending -> return (clips2 tx) 


  

-- | Deploy --------------------------------------------------
main2 :: IO ()
main2 = do
  conn <- open "vocab.db" 
  file <- T.readFile "My Clippings.txt"
  run 3031 $ (serve (Proxy @Api) (server file conn)) 
-- http://localhost:3000/clippingGetAll?listBy=Ascending&fav=true


instance ToSchema Clipping
instance ToSchema ClipFormat
instance ToSchema Vocabulary

type SwaggerUI = SwaggerSchemaUI "swagger-ui" "swagger.json"
type SWAPI = SwaggerUI :<|> Api 

swaggerApi :: Proxy SWAPI 
swaggerApi = Proxy



test :: Text  -> Connection ->  Application
test  x  c =   serve swaggerApi $
  swaggerSchemaUIServer (toSwagger (Proxy :: Proxy Api)) :<|> (server x c) 
  

run2 :: Int ->  IO ()
run2 port = do
  conn <- open "vocab.db"
  file <- T.readFile "My Clippings.txt"
  run port  (test file conn)
-- http://localhost:3000/swagger-ui/
-- to kill   kill -9 $(lsof -i:3000 -t)


run3 ::   IO ()
run3 = do
  conn <- open "vocab.db"
  file <- T.readFile "My Clippings.txt"
  run 3000  (test file conn)
-- http://localhost:3000/swagger-ui/
-- to kill   kill -9 $(lsof -i:3000 -t)
-- https://clipping-json.herokuapp.com/vocabGetBy/20/1423590816593?isMastered=false&isDeleted=false


main3 :: Int ->  IO ()
main3 port = do
  conn <- open "vocab.db"
  file <- T.readFile "My Clippings.txt"
  run port $ (serve (Proxy @Api) (server file conn))







--------------------
{-

curl -X PUT "http://localhost:3000/vocUpdate" -H "accept: application/json;charset=utf-8" -H "Content-Type: application/json;charset=utf-8" -d "[ { \"author\": \"string\", \"title\": \"string\", \"time\": 0, \"wordKey\": \"string\", \"usage\": \"string\", \"mastered\": true, \"deleted\": false }]"


curl -X DELETE "http://localhost:3000/vocDelete/12939" -H "accept: application/json;charset=utf-8"

curl -X GET "http://localhost:3000/vocabGetBy/10/4?isMastered=false&isDeleted=false&toDate=1423590816593" -H "accept: application/json;charset=utf-8"

-}
