-- | A geodetic curve is made of a distance in metres, an azimuth and a reverse azimuth.
module Data.Geo.Geodetic.Curve(
  Curve
, curve
, curveDistance
, curveAzimuth
, curveReverseAzimuth
) where

import Prelude(Eq, Show(..), Ord(..), Double, (.), abs, showString, showParen)
import Data.List(intercalate)
import Text.Printf(printf)
import Control.Lens
import Data.Geo.Geodetic.Azimuth

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
    showParen (n > 10) (showString (intercalate " " ["GeodeticCurve", printf "%0.4f" d, show a, show r]))

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
