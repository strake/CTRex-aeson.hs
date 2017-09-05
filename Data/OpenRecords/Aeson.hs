{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.OpenRecords.Aeson where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Lazy as M
import Data.List (intercalate)
import Data.OpenRecords
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Unconstrained

instance Forall r ToJSON => ToJSON (Rec r) where
    toJSON = Object . eraseToHashMap (Proxy @ToJSON) toJSON

instance (Forall r Unconstrained1, Forall r FromJSON) => FromJSON (Rec r) where
    parseJSON (Object o) = rinitAWithLabel (Proxy @FromJSON) $ \ l -> o .: (show' l)
    parseJSON v = typeMismatch msg v
      where msg = "{" ++ intercalate "," (labels (Proxy @r)) ++ "}"

show' :: Show a => a -> Text
show' = Text.pack . show
