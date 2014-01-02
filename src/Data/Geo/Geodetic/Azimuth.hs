-- | An azimuth in degrees between 0 and 360.
module Data.Geo.Geodetic.Azimuth(
  Azimuth
, HasAzimuth(..)
, modAzimuth
, nAzimuth
) where

import Prelude(Double, Bool(..), Eq, Show(..), Ord(..), id, (&&), (++), showParen, showString)
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', Lens', prism')
import Text.Printf(printf)
import Data.Fixed(mod')

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..))

newtype Azimuth =
  Azimuth Double
  deriving (Eq, Ord)

-- | A show instance that prints to 4 decimal places.
-- This is to take floating-point rounding errors into account.
instance Show Azimuth where
  showsPrec n (Azimuth d) =
    showParen (n > 10) (showString ("Azimuth " ++ printf "%0.4f" d))

-- | Construct an azimuth such that if the given value is out of bounds,
-- a modulus is taken to keep it within 0 inclusive and 360 exclusive.
--
-- >>> modAzimuth 7
-- Azimuth 7.0000
--
-- >>> modAzimuth 0
-- Azimuth 0.0000
--
-- >>> modAzimuth 360
-- Azimuth 0.0000
--
-- >>> modAzimuth 361
-- Azimuth 1.0000
--
-- >>> modAzimuth 359.999
-- Azimuth 359.9990
modAzimuth ::
  Double
  -> Azimuth
modAzimuth x =
  Azimuth (x `mod'` 360)

-- | A prism on azimuth to an integer between 0 and 359 inclusive.
--
-- >>> 7 ^? nAzimuth
-- Just (Azimuth 7.0000)
--
-- >>> 0 ^? nAzimuth
-- Just (Azimuth 0.0000)
--
-- >>> 359.999 ^? nAzimuth
-- Just (Azimuth 359.9990)
--
-- >>> 360 ^? nAzimuth
-- Nothing
--
-- prop> all (\m -> nAzimuth # m == n) (n ^? nAzimuth)
nAzimuth ::
  Prism' Double Azimuth
nAzimuth =
  prism'
    (\(Azimuth i) -> i)
    (\i -> case i >= 0 && i < 360 of
             True -> Just (Azimuth i)
             False -> Nothing)

class HasAzimuth t where
  azimuth ::
    Lens' t Azimuth

instance HasAzimuth Azimuth where
  azimuth =
    id
