{-# LANGUAGE NoImplicitPrelude #-}

-- | Great circle geodetic distance algorithm.
module Data.Geo.Geodetic.GreatCircle {- (
  sphericalLaw
, sphericalLawD
, sphericalLaw'
) -} where

import Control.Lens
import Data.Geo.Geodetic.Sphere
import Data.Geodetic.LL
import Prelude

sphericalLaw ::
  (HasLL ll1, HasLL ll2) =>
  Sphere -- ^ reference sphere
  -> ll1 -- ^ start coordinate
  -> ll2 -- ^ end coordinate
  -> Double
sphericalLaw s start end =
  let lat1 = start ^. lat
      lon1 = start ^. lon
      lat2 = end ^. lat
      lon2 = end ^. lon
  in acos (sin lat1 * sin lat2 + cos lat1 * cos lat2 * cos (lon2 - lon1)) * _Sphere # s


{-
import Control.Applicative(Const)
import Control.Lens((#), (^.))
import Data.Geo.Coordinate(AsCoordinate(_Coordinate), Coordinate, AsLongitude(_Longitude), AsLatitude(_Latitude))
import Data.Geo.Geodetic.Sphere(AsSphere(_Sphere), Sphere, earthMean)
import Prelude(Double, Num((*), (+), (-)), Fractional((/)), pi, sin, cos, acos)
import System.Args.Optional(Optional1(optional1))

-- $setup
-- >>> import Prelude(Functor(fmap), Monad(return), String)
-- >>> import Data.Geo.Coordinate((<°>))
-- >>> import Data.Maybe(Maybe)
-- >>> import Text.Printf(printf)

-- | Great circle spherical law algorithm.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (sphericalLaw earthMean fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (sphericalLaw earthMean fr to)) :: Maybe String
-- Just "17128743.0669"
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (sphericalLaw ((6350000 :: Double) ^. _Sphere) fr to)) :: Maybe String
-- Just "14959840.4461"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (sphericalLaw ((6350000 :: Double) ^. _Sphere) fr to)) :: Maybe String
-- Just "17081801.7377"
sphericalLaw ::
  (AsCoordinate (->) (Const Coordinate) start, AsCoordinate (->) (Const Coordinate) end) =>
  Sphere -- ^ reference sphere
  -> start -- ^ start coordinate
  -> end -- ^ end coordinate
  -> Double
sphericalLaw s start' end' =
  let start = start' ^. _Coordinate
      end = end' ^. _Coordinate
      toRadians n = n * pi / 180
      lat1 = toRadians (_Latitude # (start ^. _Latitude))
      lat2 = toRadians (_Latitude # (end ^. _Latitude))
      lon1 = toRadians (_Longitude # (start ^. _Longitude))
      lon2 = toRadians (_Longitude # (end ^. _Longitude))
  in acos (sin lat1 * sin lat2 + cos lat1 * cos lat2 * cos (lon2 - lon1)) * _Sphere # s

-- | Great circle spherical law algorithm with a default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (sphericalLawD fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (sphericalLawD fr to)) :: Maybe String
-- Just "17128743.0669"
sphericalLawD ::
  (AsCoordinate (->) (Const Coordinate) start, AsCoordinate (->) (Const Coordinate) end) =>
  start -- ^ start coordinate
  -> end -- ^ end coordinate
  -> Double
sphericalLawD =
  sphericalLaw earthMean

-- | Great circle spherical law algorithm with an optionally applied default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (sphericalLaw' fr to :: Double)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (sphericalLaw' fr to :: Double)) :: Maybe String
-- Just "17128743.0669"
sphericalLaw' ::
  (Optional1
    Sphere
    (
      Coordinate
      -> Coordinate
      -> Double
    ) x) =>
    x
sphericalLaw' =
  optional1 (sphericalLaw :: Sphere -> Coordinate -> Coordinate -> Double) earthMean
-}
