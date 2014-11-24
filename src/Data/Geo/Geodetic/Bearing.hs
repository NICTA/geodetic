{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A bearing in degrees between 0 and 360.
module Data.Geo.Geodetic.Bearing(
  Bearing
, AsBearing(..)
, modBearing
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id))
import Control.Lens(Choice, Optic', prism')
import Data.Bool(bool, (&&))
import Data.Eq(Eq)
import Data.Fixed(mod')
import Data.List((++))
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord((>), (>=), (<)))
import Prelude(Double, Show(showsPrec), showString, showParen)
import Text.Printf(printf)

-- $setup
-- >>> import Control.Lens((#), (^?))
-- >>> import Data.Eq(Eq((==)))
-- >>> import Data.Foldable(all)
-- >>> import Prelude(Num((*), (-)), Floating(pi))

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

