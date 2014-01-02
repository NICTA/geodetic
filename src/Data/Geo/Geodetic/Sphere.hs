-- | A sphere with a radius in metres.
module Data.Geo.Geodetic.Sphere(
  Sphere
, HasSphere(..)
, nSphere
, earthMean
) where

import Prelude(Double, Bool(..), Eq, Show(..), Ord(..), id, (&&), (++), (.), abs, showParen, showString)
import Control.Lens(Iso', Lens', iso)
import Text.Printf(printf)

-- $setup
-- >>> import Control.Lens((#), (^.))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype Sphere =
  Sphere Double
  deriving (Eq, Ord)

-- | A show instance that prints to 4 decimal places.
-- This is to take floating-point rounding errors into account.
instance Show Sphere where
  showsPrec n (Sphere d) =
    showParen (n > 10) (showString ("Sphere " ++ printf "%0.4f" d))

-- | An isomorphism on sphere to a double.
--
-- >>> 7 ^. nSphere
-- Sphere 7.0000
--
-- >>> 0 ^. nSphere
-- Sphere 0.0000
--
-- >>> (-7) ^. nSphere
-- Sphere -7.0000
nSphere ::
  Iso' Double Sphere
nSphere =
  iso Sphere (\(Sphere d) -> d)

earthMean ::
  Sphere
earthMean =
  Sphere 6367450

class HasSphere t where
  sphere ::
    Lens' t Sphere

instance HasSphere Sphere where
  sphere =
    id
