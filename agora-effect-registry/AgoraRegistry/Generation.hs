{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module AgoraRegistry.Generation (
  PHasDatumSchema,
  HasDatumSchema (..),
) where

import AgoraRegistry.Generation.Plutarch (PHasDatumSchema (pdatumSchema))
import AgoraRegistry.Schema (DatumSchema)
import Data.Kind (Type)
import Plutarch.Lift (PConstantDecl (PConstanted))

-- | @since 1.0.0
class HasDatumSchema (h :: Type) where
  datumSchema :: DatumSchema
  default datumSchema ::
    ( PHasDatumSchema (PConstanted h)
    ) =>
    DatumSchema
  datumSchema = pdatumSchema @(PConstanted h)
