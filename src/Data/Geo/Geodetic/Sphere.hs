{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- | A sphere with a radius in metres.
module Data.Geo.Geodetic.Sphere(
  Sphere
, AsSphere(..)
, earthMean
) where

import Prelude(Double, Eq, Show(..), Ord(..), id, (++), showParen, showString)
import Control.Lens(Optic', Profunctor, iso)
import Data.Functor(Functor)
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

earthMean ::
  Sphere
earthMean =
  Sphere 6367450

class AsSphere p f s where
  _Sphere ::
    Optic' p f s Sphere

instance AsSphere p f Sphere where
  _Sphere =
    id

-- | An isomorphism on sphere to a double.
--
-- >>> (7 :: Double) ^. _Sphere
-- Sphere 7.0000
--
-- >>> (0 :: Double) ^. _Sphere
-- Sphere 0.0000
--
-- >>> (-7 :: Double) ^. _Sphere
-- Sphere -7.0000
instance (Functor f, Profunctor p) => AsSphere p f Double where
  _Sphere =
    iso 
      Sphere
      (\(Sphere d) -> d)
