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

instance Forall r ToJSON => ToJSON (Rec r) where
    toJSON = Object . eraseToHashMap (Proxy @ToJSON) toJSON

instance Forall r FromJSON => FromJSON (Rec r) where
    parseJSON (Object o) = rinitAWithLabel (Proxy @FromJSON) $ \ l -> o .: (show' l)
    parseJSON v = typeMismatch msg v
      where
        msg = "{" ++ intercalate "," fields ++ "}"
        fields = labels @r @FromJSON $ rinit (Proxy @FromJSON) undefined

show' :: Show a => a -> Text
show' = Text.pack . show

-- DUPLICATED because @Data.OpenRecords@ does not export it.
labels :: forall r c. Forall r c => Rec r -> [String]
labels = fmap fst . eraseWithLabels (Proxy @c) (\ _ -> undefined)
