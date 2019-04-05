{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}



module One where

import Servant 
import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp (run)

  
data User = User
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

type UserAPI = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User
         
isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" 

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" 

users :: [User]
users = [isaac, albert]

server :: Server UserAPI
server =  return [ User "Albert Einstein" 136 "ae@mc2.org"]
     :<|> return albert
     :<|> return isaac

userAPI :: Proxy UserAPI
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server


main1 :: IO ()
main1 = run 8081 app1


