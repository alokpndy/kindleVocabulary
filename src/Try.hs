{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Try where

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Typeable (Typeable)
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import GHC.Generics

import Servant ((:<|>) (..), (:>) (..), Server, Application, serve, JSON, Get, Capture, Handler (..))

{-
--               \/ TypeOperators
data (path :: k) :> (a :: *) deriving (Typeable)
--            ^ Polykinds
infixr 4 :>
-}

type family Endpoint api where
  Endpoint (a :<|> b) = AppendList (Endpoint a) (Endpoint b)
  -- On RHS              ^ ---------------------------------
  -- we have type function rather that a type, compiler cannot
  -- ensure that it will terminate. USE -XUndecidableinstances
  Endpoint (e :> a)   = MapSub e (Endpoint a)
  Endpoint a          = '[a]  

type family AppendList xy ys where
  AppendList '[] ys = ys
  AppendList (x ': xs) ys = x ': AppendList xs ys

type family MapSub e xs where
  MapSub e '[] = '[]
  MapSub e (x ': xs) = (e :> x) ': MapSub e xs

{-
type Capture = Capture' '[]
data Capture' (mods :: [*]) (sym :: Symbol) (a :: *)
    deriving (Typeable)
-- type MyAPI = "src" :> CaptureAll "segments" Text :> Get '[JSON] SourceFile
data CaptureAll (sym :: Symbol) (a :: *)
    deriving (Typeable)
-}

type family IsElem' a s :: Constraint

type family IsElem endpoint api :: Constraint where
  IsElem e (sa :<|> sb)                   = Or (IsElem e sa) (IsElem e sb)
  IsElem (e :> sa) (e :> sb)              = IsElem sa sb
  IsElem e e                              = ()
  IsElem e a                              = IsElem' e a
  IsElem (Capture z y :> sa) (Capture x y :> sb)
                                          = IsElem sa sb

type family Or (a:: Constraint) (b:: Constraint) :: Constraint  where
  Or () b = ()
  Or a () = ()
type family And (a:: Constraint) (b:: Constraint) :: Constraint  where
  And () () = ()

type family IsSubApi sub api :: Constraint where
  IsSubApi sub api = AllIsElem (Endpoint sub) api
  
type family AllIsElem xs api :: Constraint where
  AllIsElem '[] api = ()
  AllIsElem (x ': xs) api = (IsElem x api, AllIsElem xs api) 

type family IsSubList a b :: Constraint where
  IsSubList '[] b = ()
  IsSubList (x ': xs) y = Elem x y `And` IsSubList xs y

type Elem e es = ElemGo e es es
--              ^ is Constraint hence Constraint Kinds

-- 'orig' is used to store original list for better error messages
type family ElemGo e es orig :: Constraint where
  ElemGo x (x ': xs) orig = ()
  ElemGo y (x ': xs) orig = ElemGo y xs orig
  -- Note [Custom Errors]
  ElemGo x '[] orig       = TypeError ('ShowType x
                                 ':<>: 'Text " expected in list "
                                 ':<>: 'ShowType orig)

{-
data RequestMethod = GET | POST | DELETE
    deriving(Eq, Show)
data Verb (method :: k1) (statusCode :: Nat) (contentTypes :: [*]) (a :: *)
  deriving (Typeable, Generic)
type Get    = Verb 'GET    200
  -}

data Person = Person { name :: String }  deriving (Eq, Show, Generic)
instance ToJSON Person
alok :: Person
alok = Person "Alok"
user :: Person
user = alok
users :: [Person]
users = [alok]
type SampleApi = "users" :> Get '[JSON] [Person]
   :<|> "user" :> Capture "name" String :> Get '[JSON] [Person] -- GET /user/:userid'


server :: Server SampleApi
server = return users
      :<|> maybeUser
      where
        maybeUser :: String -> Handler [Person]
        maybeUser = (\s -> return (filter (\x -> name x == s)  users))


-- Servant.Server.serve :: HasServer api '[] =>
--     Proxy api -> ServerT api Handler -> Application
app1 :: Application
app1 = serve (Proxy @SampleApi) server


main1 :: IO ()
main1 = run 8081 app1

{-
newtype Handler a = Handler { runHandler' :: ExceptT ServantErr IO a }
data ServantErr = ServantErr { errHTTPCode     :: Int
                             , errReasonPhrase :: String
                             , errBody         :: LBS.ByteString
                             , errHeaders      :: [HTTP.Header]
                             } deriving (Show, Eq, Read, Typeable)
------------------------------------------------------------------
more about HasService : https://github.com/haskell-servant/servant/blob/3db3d38e14a803c11f13782a23aba3da6c7de570/servant-server/src/Servant/Server/Internal.hs

class HasServer api context where
  type ServerT api (m :: * -> *) :: *

  route ::
       Proxy api
    -> Context context
    -> Delayed env (Server api)
    -> Router env

  hoistServerWithContext
      :: Proxy api
      -> Proxy context
      -> (forall x. m x -> n x)
      -> ServerT api m
      -> ServerT api n

type Server api = ServerT api Handler
-}


