{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Servant
--------------------------------------------------------------------------------
import Api.User
import Domain.User
--------------------------------------------------------------------------------

server :: Server UserApi
server = hoistServer usersApi runDomainInMemory usersServer

app :: Application
app = serve usersApi server

exec :: IO ()
exec = W.run 8080 app
