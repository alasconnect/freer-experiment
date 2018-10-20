{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import Domain.User
import Models.User (User, UserId)
--------------------------------------------------------------------------------

type GetUsersApi
  = Get '[JSON] [User]

type GetUserApi
  =  Capture "user_id" UserId
  :> Get '[JSON] (Maybe User)

type CreateUserApi
  =  ReqBody '[JSON] User
  :> Post '[JSON] User

type UpdateUserApi
  =  ReqBody '[JSON] User
  :> Put '[JSON] NoContent

type DeleteUserApi
  =  Capture "user_id" UserId
  :> Delete '[JSON] NoContent

type UserApi
  =  "api"
  :> "v1"
  :> "users"
  :> (    GetUsersApi
     :<|> GetUserApi
     :<|> CreateUserApi
     :<|> UpdateUserApi
     :<|> DeleteUserApi
     )

usersApi :: Proxy UserApi
usersApi = Proxy

usersServer :: Members '[DomainUser] effs => ServerT UserApi (Eff effs)
usersServer = usersGet :<|> userGet :<|> userCreate :<|> userUpdate :<|> userDelete

runDomainInMemory :: Eff '[DomainUser, Handler] a -> Handler a
runDomainInMemory = runM . runDomainUserInMemory

usersGet :: Members '[DomainUser] effs => Eff effs [User]
usersGet = getUsers

userGet :: Members '[DomainUser] effs => UserId -> Eff effs (Maybe User)
userGet = getUser

userCreate :: Members '[DomainUser] effs => User -> Eff effs User
userCreate = createUser

userUpdate :: Members '[DomainUser] effs => User -> Eff effs NoContent
userUpdate u = updateUser u >> return NoContent

userDelete :: Members '[DomainUser] effs => UserId -> Eff effs NoContent
userDelete uid = deleteUser uid >> return NoContent
