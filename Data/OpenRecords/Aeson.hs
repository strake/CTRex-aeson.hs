{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.OpenRecords.Aeson where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Lazy as M
import Data.OpenRecords
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text (pack)

instance Forall r ToJSON => ToJSON (Rec r) where
    toJSON = Object . eraseToHashMap (Proxy @ToJSON) toJSON

instance Forall r FromJSON => FromJSON (Rec r) where
    parseJSON (Object o) = rinitAWithLabel (Proxy @FromJSON) $ \ l -> case M.lookup (show' l) o of
        Nothing -> typeMismatch "" (Object o)
        Just v -> parseJSON v
    parseJSON v = typeMismatch "" v

show' :: Show a => a -> Text
show' = Text.pack . show
