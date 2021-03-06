{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- | A geodetic curve is made of a distance in metres, an azimuth and a reverse azimuth.
module Data.Geo.Geodetic.Curve(
  Curve
, AsCurve(..)
, curve
, curveDistance
, curveAzimuth
, curveReverseAzimuth
) where

import Control.Category(id)
import Control.Lens(Optic', Lens', Profunctor, lens, iso)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.List(unwords)
import Data.Ord(Ord((>)))
import Data.Geo.Geodetic.Azimuth
import Text.Printf(printf)
import Prelude(Show(show, showsPrec), Double, showString, showParen)

data Curve =
  Curve
    Double -- The ellipsoidal distance.
    Azimuth -- The azimuth.
    Azimuth -- The reverse azimuth.
  deriving (Eq, Ord)

-- | A show instance that prints to 4 decimal places.
-- This is to take floating-point rounding errors into account.
instance Show Curve where
  showsPrec n (Curve d a r) =
    showParen (n > 10) (showString (unwords ["GeodeticCurve", printf "%0.4f" d, show a, show r]))

-- | Construct a geodetic curve with the given parameters.
curve ::
  Double -- ^ The ellipsoidal distance.
  -> Azimuth -- ^ The azimuth.
  -> Azimuth -- ^ The reverse azimuth.
  -> Curve
curve =
  Curve

curveDistance ::
  Lens' Curve Double
curveDistance =
  lens (\(Curve d _ _) -> d) (\(Curve _ a r) d -> Curve d a r)

curveAzimuth ::
  Lens' Curve Azimuth
curveAzimuth =
  lens (\(Curve _ a _) -> a) (\(Curve d _ r) a -> Curve d a r)

curveReverseAzimuth ::
  Lens' Curve Azimuth
curveReverseAzimuth =
  lens (\(Curve _ _ r) -> r) (\(Curve d a _) r -> Curve d a r)

class AsCurve p f s where
  _Curve ::
    Optic' p f s Curve

instance AsCurve p f Curve where
  _Curve =
    id

instance (Profunctor p, Functor f) => AsCurve p f (Double, Azimuth, Azimuth) where
  _Curve =
    iso
      (\(d, a, r) -> Curve d a r)
      (\(Curve d a r) -> (d, a, r))
