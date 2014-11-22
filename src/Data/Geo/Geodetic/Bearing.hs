{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A bearing in degrees between 0 and 360.
module Data.Geo.Geodetic.Bearing(
  Bearing
, AsBearing(..)
, modBearing
) where

import Control.Applicative(Applicative)
import Prelude(Floating, Double, Eq, Show(..), Ord(..), id, (&&), (++), (.), showString, showParen)
import Data.Bool(bool)
import Data.Maybe(Maybe(..))
import Data.Radian(Radian, radians)
import Control.Lens(Choice, Optic', prism')
import Text.Printf(printf)
import Data.Fixed(mod')

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Eq(..), Num((*), (-)), Floating(pi))

newtype Bearing =
  Bearing Double
  deriving (Eq, Ord)

-- | A show instance that prints to 4 decimal places.
-- This is to take floating-point rounding errors into account.
instance Show Bearing where
  showsPrec n (Bearing d) =
    showParen (n > 10) (showString ("Bearing " ++ printf "%0.4f" d))

-- | Construct a bearing such that if the given value is out of bounds,
-- a modulus is taken to keep it within 0 inclusive and 360 exclusive.
--
-- >>> modBearing 7
-- Bearing 7.0000
--
-- >>> modBearing 0
-- Bearing 0.0000
--
-- >>> modBearing (-0.0001)
-- Bearing 359.9999
--
-- >>> modBearing 360
-- Bearing 0.0000
--
-- >>> modBearing 359.99999
-- Bearing 360.0000
--
-- >>> modBearing 359.999
-- Bearing 359.9990
modBearing ::
  Double
  -> Bearing
modBearing x =
  Bearing (x `mod'` 360)

class AsBearing p f s where
  _Bearing ::
    Optic' p f s Bearing

instance AsBearing p f Bearing where
  _Bearing =
    id

-- | A prism on bearing to a double between 0 inclusive and 360 exclusive.
--
-- >>> (7 :: Double) ^? _Bearing
-- Just (Bearing 7.0000)
--
-- >>> (0 :: Double) ^? _Bearing
-- Just (Bearing 0.0000)
--
-- >>> (359 :: Double) ^? _Bearing
-- Just (Bearing 359.0000)
--
-- >>> (359.997 :: Double) ^? _Bearing
-- Just (Bearing 359.9970)
--
-- >>> (360 :: Double) ^? _Bearing
-- Nothing
--
-- prop> all (\m -> _Bearing # m == n) ((n :: Double) ^? _Bearing)
instance (Choice p, Applicative f) => AsBearing p f Double where
  _Bearing =
    prism'
      (\(Bearing i) -> i)
      (\i -> bool Nothing (Just (Bearing i)) (i >= 0 && i < 360))

-- | A prism on bearing to a double between 0 and π exclusive.
--
-- >>> (2 * pi - 0.0000000001 :: Radian Double) ^? _Bearing
-- Just (Bearing 360.0000)
--
-- >>> (0 :: Radian Double) ^? _Bearing
-- Just (Bearing 0.0000)
--
-- >>> (0.001 :: Radian Double) ^? _Bearing
-- Just (Bearing 0.0573)
--
-- >>> (1.78391 :: Radian Double) ^? _Bearing
-- Just (Bearing 102.2105)
--
-- >>> (pi :: Radian Double) ^? _Bearing
-- Just (Bearing 180.0000)
--
-- >>> (2 * pi :: Radian Double) ^? _Bearing
-- Nothing
--
-- >>> (-0.001 :: Radian Double) ^? _Bearing
-- Nothing
instance (Choice p, Applicative f, Floating x, AsBearing p f x) => AsBearing p f (Radian x) where
  _Bearing =
    radians . _Bearing
