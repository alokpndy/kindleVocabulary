{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Upload where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 hiding (main)

data Page = Page

instance ToMarkup Page where
  toMarkup Page = h1 "hello world"

type APIHTML = Get '[HTML] Page

apiHT :: Proxy APIHTML
apiHT = Proxy

server :: Server APIHTML
server = return Page

app :: Application
app = serve apiHT server

main :: IO ()
main = run 8080 app
