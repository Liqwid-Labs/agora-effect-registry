{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module AgoraRegistry.Generation (
  PHasDatumSchema,
  HasDatumSchema (..),
  exportSchema,
  exportEffectSchema,
) where

import AgoraRegistry.Generation.Plutarch (
  PHasDatumSchema (pdatumSchema),
 )
import AgoraRegistry.Schema (
  DatumSchema,
  EffectSchema (EffectSchema),
  Metadata,
  Schema' (Schema),
 )
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Plutarch.Lift (PConstantDecl (PConstanted))

-- | @since 1.0.0
class HasDatumSchema (h :: Type) where
  datumSchema :: DatumSchema
  default datumSchema ::
    ( PHasDatumSchema (PConstanted h)
    ) =>
    DatumSchema
  datumSchema = pdatumSchema @(PConstanted h)

-- | @since 1.0.0
exportSchema ::
  forall (a :: Type).
  HasDatumSchema a =>
  Maybe Metadata ->
  FilePath ->
  IO ()
exportSchema meta =
  flip
    Aeson.encodeFile
    $ Schema meta (datumSchema @a)

-- | @since 1.0.0
exportEffectSchema ::
  forall (a :: Type).
  HasDatumSchema a =>
  Metadata ->
  Maybe ByteString ->
  FilePath ->
  IO ()
exportEffectSchema meta scriptHash =
  flip Aeson.encodeFile $
    EffectSchema meta (fromMaybe "" scriptHash) $
      Schema Nothing $
        datumSchema @a
