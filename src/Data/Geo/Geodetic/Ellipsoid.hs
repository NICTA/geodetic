{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Geo.Geodetic.Ellipsoid(
  -- * Data type
  Ellipsoid
, AsEllipsoid(..)
  -- * Ellipsoid properties
, AsSemiMajor(..)
, AsSemiMinor(..)
, AsFlattening(..)
, AsInverseFlattening(..)
  -- * Ellipsoid construction
, semiMajorFlattening
, semiMinorFlattening
, semiMajorInverseFlattening
, semiMinorInverseFlattening
  -- * Ellipsoids
, wgs84
, grs80
, grs67
, ans
, wgs72
, au1965
, krasovsky1940
, international1924
, hayford1909
, airy1830
, everest1830
, bessel1841
, clarke1858
, clarke1866
, clarke1880
) where

import Prelude(Eq, Ord, Show, Num(..), Fractional(..), Double, id, Functor)
import Control.Lens(Optic', lens)

data Ellipsoid =
  Ellipsoid
    Double -- The semi major axis in metres.
    Double -- The ellipsoidal flattening.
  deriving (Eq, Ord, Show)

semiMajorFlattening ::
  Double -- ^ The semi major axis in metres.
  -> Double -- ^ The ellipsoidal flattening.
  -> Ellipsoid
semiMajorFlattening =
  Ellipsoid

semiMinorFlattening ::
  Double -- ^ The semi minor axis in metres.
  -> Double -- ^ The ellipsoidal flattening.
  -> Ellipsoid
semiMinorFlattening s f =
  Ellipsoid (s * f - 1.0) f

semiMajorInverseFlattening ::
  Double -- ^ The semi major axis in metres.
  -> Double -- ^ The ellipsoidal inverse flattening.
  -> Ellipsoid
semiMajorInverseFlattening s f =
  semiMajorFlattening s (1/f)

semiMinorInverseFlattening ::
  Double -- ^ The semi minor axis in metres.
  -> Double -- ^ The ellipsoidal inverse flattening.
  -> Ellipsoid
semiMinorInverseFlattening s f =
  semiMinorFlattening s (1/f)

class AsEllipsoid p f s where
  _Ellipsoid ::
    Optic' p f s Ellipsoid    

instance AsEllipsoid p f Ellipsoid where
  _Ellipsoid =
    id

class AsSemiMajor p f s where
  _SemiMajor ::
    Optic' p f s Double

instance AsSemiMajor p f Double where
  _SemiMajor =
    id

instance (p ~ (->), Functor f) => AsSemiMajor p f Ellipsoid where
  _SemiMajor =
     lens (\(Ellipsoid s _) -> s) (\(Ellipsoid _ f) s -> Ellipsoid s f)   

class AsSemiMinor p f s where
  _SemiMinor ::
    Optic' p f s Double

instance AsSemiMinor p f Double where
  _SemiMinor =
    id    

instance (p ~ (->), Functor f) => AsSemiMinor p f Ellipsoid where
  _SemiMinor =
     lens (\(Ellipsoid s f) -> (1.0 - f) * s) (\(Ellipsoid _ f) s -> Ellipsoid (s * f - 1.0) f)

class AsFlattening p f s where
  _Flattening ::
    Optic' p f s Double

instance AsFlattening p f Double where
  _Flattening =
    id

instance (p ~ (->), Functor f) => AsFlattening p f Ellipsoid where
  _Flattening =
    lens (\(Ellipsoid _ f) -> f) (\(Ellipsoid s _) f -> Ellipsoid s f)

class AsInverseFlattening p f s where
  _InverseFlattening ::
    Optic' p f s Double

instance AsInverseFlattening p f Double where
  _InverseFlattening =
    id

instance (p ~ (->), Functor f) => AsInverseFlattening p f Ellipsoid where
  _InverseFlattening =
    lens (\(Ellipsoid _ f) -> f) (\(Ellipsoid s _) f -> Ellipsoid s f)

wgs84 ::
  Ellipsoid
wgs84 =
  semiMajorInverseFlattening 6378137 298.257223563

grs80 ::
  Ellipsoid
grs80 =
  semiMajorInverseFlattening 6378137 298.257222101

grs67 ::
  Ellipsoid
grs67 =
  semiMajorInverseFlattening 6378160 298.25

ans ::
  Ellipsoid
ans =
  semiMajorInverseFlattening 6378160 298.25

wgs72 ::
  Ellipsoid
wgs72 =
  semiMajorInverseFlattening 6378135 298.26

au1965 ::
  Ellipsoid
au1965 =
  semiMajorInverseFlattening 6378160 298.25

krasovsky1940 ::
  Ellipsoid
krasovsky1940 =
  semiMajorInverseFlattening 6378245 298.3

international1924 ::
  Ellipsoid
international1924 =
  semiMajorInverseFlattening 6378388 297

hayford1909 ::
  Ellipsoid
hayford1909 =
  international1924

airy1830 ::
  Ellipsoid
airy1830 =
  semiMajorInverseFlattening 6377563.4 299.32

everest1830 ::
  Ellipsoid
everest1830 =
  semiMajorInverseFlattening 6377276.3 300.8

bessel1841 ::
  Ellipsoid
bessel1841 =
  semiMajorInverseFlattening 6377397.2 299.15

clarke1858 ::
  Ellipsoid
clarke1858 =
  semiMajorInverseFlattening 6378293.645 294.26

clarke1866 ::
  Ellipsoid
clarke1866 =
  semiMajorInverseFlattening 6378206.4 294.98

clarke1880 ::
  Ellipsoid
clarke1880 =
  semiMajorInverseFlattening 6378249.145 293.465
