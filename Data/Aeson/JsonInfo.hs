{-# LANGUAGE LambdaCase, GADTs, FlexibleContexts, ScopedTypeVariables #-}
module Data.Aeson.JsonInfo where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text, unpack)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))

type JsonInfo a = JsonFieldInfo a a

-- | 'JsonFieldInfo'
--
-- Use the 'JsonInfo' type alias instead
--
-- > data R = R { a :: Text, b :: Int32 } deriving Typeable
-- >
-- > rJsonInfo :: JsonInfo R
-- > rJsonInfo = R
-- >         <$> "a" @: a
-- >         <*> 'b  @: b
-- >
-- > instance ToJSON R where
-- >   toJSON = jiToJSON rJsonInfo
-- > instance FromJSON R where
-- >   parseJSON = jiParseJSON rJsonInfo
data JsonFieldInfo t a where
  Pure :: a -> JsonFieldInfo t a
  Map :: (a -> b) -> JsonFieldInfo t a -> JsonFieldInfo t b
  App :: JsonFieldInfo t (a -> b) -> JsonFieldInfo t a -> JsonFieldInfo t b
  Field :: (ToJSON a, FromJSON a)
        => (t -> a) -- ^ the accessor
        -> Text -- ^ name in the json object
        -> Maybe a -- ^ default value
        -> JsonFieldInfo t a

instance Functor (JsonFieldInfo t) where
  fmap = Map

instance Applicative (JsonFieldInfo t) where
  pure = Pure
  (<*>) = App

fieldJInfo, (@:) :: (ToJSON a, FromJSON a) => Text -> (t -> a) -> JsonFieldInfo t a
fieldJInfo name accessor = Field accessor name Nothing
(@:) = fieldJInfo

fieldJInfoOpt, (@:?) :: (ToJSON (Maybe a), FromJSON (Maybe a)) => Text -> (t -> Maybe a) -> JsonFieldInfo t (Maybe a)
fieldJInfoOpt name accessor = Field accessor name (Just Nothing)
(@:?) = fieldJInfoOpt

jiToJSON :: JsonInfo t -> t -> Value
jiToJSON ji = Object . jiToObject ji

jiToObject :: forall t. JsonInfo t -> t -> Object
jiToObject ji t = mkObj ji HM.empty
  where
    mkObj :: JsonFieldInfo t a -> Object -> Object
    mkObj = \case
      Pure _ -> id
      Map _ x -> mkObj x
      App x y -> mkObj x . mkObj y
      Field accessor name _ -> HM.insert name . toJSON $ accessor t

jiParseJSON :: Typeable t => JsonInfo t -> Value -> Parser t
jiParseJSON (ji :: JsonInfo t) =
    withObject (show $ typeRep (Proxy :: Proxy t))
  $ jiFromObject ji

jiFromObject :: forall t. JsonInfo t -> Object -> Parser t
jiFromObject ji o = go ji
  where
    go :: forall a. JsonFieldInfo t a -> Parser a
    go = \case
      Pure x -> pure x
      Map f x -> f <$> go x
      App x y -> go x <*> go y
      Field _ name def -> case HM.lookup name o of
          Just v -> parseJSON v
          Nothing -> maybe (fail $ "key " <> unpack name <> " not present") return def
