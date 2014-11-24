{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Haversine geodetic distance algorithm.
module Data.Geo.Geodetic.Haversine(
  haversine
, haversineD
, haversine'
) where

import Control.Applicative(Const)
import Control.Category(Category((.)))
import Control.Lens((#), (^.))
import System.Args.Optional(Optional1(optional1))
import Data.Geo.Coordinate(AsCoordinate(_Coordinate), Coordinate, AsLongitude(_Longitude), AsLatitude(_Latitude))
import Data.Geo.Geodetic.Sphere(AsSphere(_Sphere), Sphere, earthMean)
import Prelude(Double, Num((*), (-), (+)), Fractional((/)), pi, sin, atan2, cos, sqrt)

-- $setup
-- >>> import Prelude(Functor(..), Monad(..), String, Double)
-- >>> import Data.Geo.Coordinate((<°>))
-- >>> import Data.Maybe(Maybe)
-- >>> import Text.Printf(printf)

-- | Haversine algorithm.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (haversine earthMean fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (haversine earthMean fr to)) :: Maybe String
-- Just "17128743.0669"
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (haversine ((6350000 :: Double) ^. _Sphere) fr to)) :: Maybe String
-- Just "14959840.4461"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (haversine ((6350000 :: Double) ^. _Sphere) fr to)) :: Maybe String
-- Just "17081801.7377"
haversine ::
  (AsCoordinate (->) (Const Coordinate) start, AsCoordinate (->) (Const Coordinate) end) =>
  Sphere -- ^ reference sphere
  -> start -- ^ start coordinate
  -> end -- ^ end coordinate
  -> Double
haversine s start' end' =
  let start = start' ^. _Coordinate
      end = end' ^. _Coordinate
      lat1 = _Latitude # (start ^. _Latitude)
      lat2 = _Latitude # (end ^. _Latitude)
      toRadians n = n * pi / 180
      dlat = toRadians (lat1 - lat2) / 2
      dlon = toRadians (_Longitude # (start ^. _Longitude) - _Longitude # (end ^. _Longitude)) / 2
      cosr = cos . toRadians
      square x = x * x
      a = square (sin dlat) + cosr lat1 * cosr lat2 * square (sin dlon)
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in (_Sphere # s) * c

-- | Haversine algorithm with a default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (haversineD fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (haversineD fr to)) :: Maybe String
-- Just "17128743.0669"
haversineD ::
  (AsCoordinate (->) (Const Coordinate) start, AsCoordinate (->) (Const Coordinate) end) =>
  start -- ^ start coordinate
  -> end -- ^ end coordinate
  -> Double
haversineD =
  haversine earthMean

-- | Haversine algorithm with an optionally applied default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 <°> 154.295; to <- (-66.093) <°> 12.84; return (haversine' fr to :: Double)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) <°> 41.935; to <- 6.933 <°> (-162.55); return (haversine' fr to :: Double)) :: Maybe String
-- Just "17128743.0669"
haversine' ::
  (Optional1
    Sphere
    (
      Coordinate
      -> Coordinate
      -> Double
    ) x) =>
    x
haversine' =
  optional1 (haversine :: Sphere -> Coordinate -> Coordinate -> Double) earthMean
