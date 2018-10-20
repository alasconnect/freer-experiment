{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Database.User where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))
import Data.Text (Text)
--------------------------------------------------------------------------------
import Models.User
--------------------------------------------------------------------------------

data DatabaseUser r where
  GetUsers :: DatabaseUser [User]
  GetUser :: UserId -> DatabaseUser (Maybe User)
  CreateUser :: User -> DatabaseUser User
  UpdateUser :: User -> DatabaseUser ()
  DeleteUser :: UserId -> DatabaseUser ()

getUsers :: Member DatabaseUser effs => Eff effs [User]
getUsers = send GetUsers

getUser :: Member DatabaseUser effs => UserId -> Eff effs (Maybe User)
getUser = send . GetUser

createUser :: Member DatabaseUser effs => User -> Eff effs User
createUser = send . CreateUser

updateUser :: Member DatabaseUser effs => User -> Eff effs ()
updateUser = send . UpdateUser

deleteUser :: Member DatabaseUser effs => UserId -> Eff effs ()
deleteUser = send . DeleteUser

type Vdb = Map UserId User

runDatabaseInMemory :: Vdb -> Eff (DatabaseUser ': effs) ~> Eff effs
runDatabaseInMemory vdb = evalState vdb . go
  where
    go :: Eff (DatabaseUser ': effs) ~> Eff (State Vdb ': effs)
    go = reinterpret $ \case
     GetUsers -> get >>= \vdb' -> return . Map.elems $ (vdb' :: Vdb)
     GetUser uid -> get >>= return . Map.lookup uid
     CreateUser (User _ t) -> do
       uid <- get >>= \vdb' -> return (mkUserId $ Map.size (vdb' :: Vdb) + 1)
       let u = User uid t
       modify (Map.insert uid u)
       return u
     UpdateUser u@(User uid _) -> modify (Map.adjust (const u) uid)
     DeleteUser uid -> modify (\vdb' -> Map.delete uid vdb' :: Vdb)
