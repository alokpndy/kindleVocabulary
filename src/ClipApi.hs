
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

clips2 :: Text ->  [Clipping]
clips2 file = do
   -- take 40 $ onlHighlight file
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

type ClipApi =
       "clippingGetAll" :> QueryParam "listBy" SortBy :> QueryParams "getBy" String
       :> QueryFlag "fav" :> Get '[JSON] [Clipping]
type VocApi =
      "vocabGetAll" :> Get '[JSON] [Vocabulary] 
type Api = ClipApi :<|> VocApi

         
server :: Text  -> Connection ->  Server Api
server tx c  = do
  clippingGetAll :<|> vocabGetAll
  where
    vocabGetAll ::   Handler [Vocabulary]
    vocabGetAll = do
      liftIO $  connectionHandler c
      
   
    clippingGetAll :: Maybe SortBy -> [String] ->  Bool -> Handler [Clipping]
    clippingGetAll qp xs qf = case qp of
      Nothing -> return []
      Just x -> case x of
        Ascending -> if qf == True then return  (clips2 tx)
                            else return (clips2 tx)
        Descending -> return (clips2 tx) 
   

main2 :: IO ()
main2 = do
  conn <- open "vocab.db" 
  file <- T.readFile "My Clippings.txt"
  run 3031 $ (serve (Proxy @Api) (server file conn)) 
-- http://localhost:3000/clippingGetAll?listBy=Ascending&fav=true

{-
instance ToSchema Clipping
instance ToSchema ClipFormat

type SwaggerUI = SwaggerSchemaUI "swagger-ui" "swagger.json"
type API = SwaggerUI :<|> ClipApi 

swaggerApi :: Proxy API 
swaggerApi = Proxy



test :: Text ->  Application
test  x =   serve swaggerApi $
  swaggerSchemaUIServer (toSwagger (Proxy :: Proxy ClipApi)) :<|> (server x) 
  

run2 :: Int ->  IO ()
run2 port = do
  file <- T.readFile "My Clippings.txt"
  run port  (test file)
-- http://localhost:3000/swagger-ui/
-- to kill   kill -9 $(lsof -i:3000 -t)
-}

main3 :: Int ->  IO ()
main3 port = do
  conn <- open "vocab.db"
  file <- T.readFile "My Clippings.txt"
  run port $ (serve (Proxy @Api) (server file conn))
