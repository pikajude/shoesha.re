{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.User where

import Model
import Prelude ((.), Maybe(Just))
import Yesod.Auth.HashDB (HashDBUser(..))

instance HashDBUser User where
    userPasswordHash = Just . userPassword
    userPasswordSalt = Just . userSalt
    setSaltAndPasswordHash s h u = u
        { userSalt     = s
        , userPassword = h
        }
