{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Domain.User where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))
import Data.Text (Text)
--------------------------------------------------------------------------------
import Database.User (DatabaseUser, runDatabaseInMemory)
import qualified Database.User as U
import Models.User (User, UserId)
--------------------------------------------------------------------------------

data DomainUser r where
  GetUsers :: DomainUser [User]
  GetUser :: UserId -> DomainUser (Maybe User)
  CreateUser :: User -> DomainUser User
  UpdateUser :: User -> DomainUser ()
  DeleteUser :: UserId -> DomainUser ()

getUsers :: Member DomainUser effs => Eff effs [User]
getUsers = send GetUsers

getUser :: Member DomainUser effs => UserId -> Eff effs (Maybe User)
getUser = send . GetUser

createUser :: Member DomainUser effs => User -> Eff effs User
createUser = send . CreateUser

updateUser :: Member DomainUser effs => User -> Eff effs ()
updateUser = send . UpdateUser

deleteUser :: Member DomainUser effs => UserId -> Eff effs ()
deleteUser = send . DeleteUser

runDomainUserInMemory :: Eff (DomainUser ':  effs) a -> Eff effs a
runDomainUserInMemory = runDatabaseInMemory Map.empty . go
  where
    go :: Eff (DomainUser ': effs) a -> Eff (DatabaseUser ': effs) a
    go = reinterpret $ \case
      GetUsers       -> U.getUsers
      GetUser uid    -> U.getUser uid
      CreateUser u   -> U.createUser u
      UpdateUser u   -> U.updateUser u
      DeleteUser uid -> U.deleteUser uid
