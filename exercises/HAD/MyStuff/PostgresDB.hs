{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module DB where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import           Database.Beam


data UserT f = User
    { _id    :: C f Int
    , _name  :: C f Text.Text
    , _email :: C f Text.Text
    } deriving (Generic)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show UserId
deriving instance Show User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . (_id :: UserT f -> C f Int)

instance Beamable UserT
instance Beamable (PrimaryKey UserT)
