module Data.Geo.Geodetic.Ellipsoid(
  -- * Data type
  Ellipsoid
, HasEllipsoid(..)
  -- * Ellipsoid properties
, HasSemiMajor(..)
, HasSemiMinor(..)
, HasFlattening(..)
, HasInverseFlattening(..)
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

import Prelude(Eq, Ord, Show, Num(..), Fractional(..), Double, id)
import Control.Lens(Lens', lens)

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

class HasEllipsoid t where
  ellipsoid ::
    Lens' t Ellipsoid

instance HasEllipsoid Ellipsoid where
  ellipsoid =
    id

class HasSemiMajor t where
  semiMajor ::
    Lens' t Double

instance HasSemiMajor Double where
  semiMajor =
    id

instance HasSemiMajor Ellipsoid where
  semiMajor =
    lens (\(Ellipsoid s _) -> s) (\(Ellipsoid _ f) s -> Ellipsoid s f)

class HasSemiMinor t where
  semiMinor ::
    Lens' t Double

instance HasSemiMinor Double where
  semiMinor =
    id

instance HasSemiMinor Ellipsoid where
  semiMinor =
    lens (\(Ellipsoid s f) -> (1.0 - f) * s) (\(Ellipsoid _ f) s -> Ellipsoid (s * f - 1.0) f)

class HasFlattening t where
  flattening ::
    Lens' t Double

instance HasFlattening Double where
  flattening =
    id

instance HasFlattening Ellipsoid where
  flattening =
    lens (\(Ellipsoid _ f) -> f) (\(Ellipsoid s _) f -> Ellipsoid s f)

class HasInverseFlattening t where
  inverseFlattening ::
    Lens' t Double

instance HasInverseFlattening Double where
  inverseFlattening =
    id

instance HasInverseFlattening Ellipsoid where
  inverseFlattening =
    lens (\(Ellipsoid _ f) -> 1/f) (\(Ellipsoid s _) f -> Ellipsoid s (1/f))

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
