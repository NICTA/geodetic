{-# LANGUAGE FlexibleContexts #-}

-- | Haversine geodetic distance algorithm.
module Data.Geo.Geodetic.Haversine(
  haversine
, haversineD
, haversine'
) where

import Prelude(Double, Num(..), Fractional(..), (.), pi, atan, sin, atan2, cos, sqrt)
import Control.Lens((#), (^.))
import System.Args.Optional(Optional1(..))
import Data.Geo.Coordinate
import Data.Geo.Geodetic.Sphere

-- $setup
-- >>> import Prelude(Functor(..), Monad(..), String)
-- >>> import Data.Maybe(Maybe)
-- >>> import Text.Printf(printf)

-- | Haversine algorithm.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (haversine earthMean fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (haversine earthMean fr to)) :: Maybe String
-- Just "17128743.0669"
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (haversine (6350000 ^. nSphere) fr to)) :: Maybe String
-- Just "14959840.4461"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (haversine (6350000 ^. nSphere) fr to)) :: Maybe String
-- Just "17081801.7377"
{-
haversine ::
  (HasCoordinate c1, HasCoordinate c2) =>
  Sphere -- ^ reference sphere
  -> c1 -- ^ start coordinate
  -> c2 -- ^ end coordinate
  -> Double
  -}
haversine s start' end' =
  let start = undef -- start' ^. coordinate
      end = undef -- end' ^. coordinate
      lat1 = undef -- fracLatitude # (start ^. latitude)
      lat2 = undef -- fracLatitude # (end ^. latitude)
      toRadians n = n * pi / 180
      dlat = (toRadians (lat1 - lat2)) / 2
      dlon = undef -- (toRadians (fracLongitude # (start ^. longitude) - fracLongitude # (end ^. longitude))) / 2
      cosr = cos . toRadians
      square x = x * x
      a = square (sin dlat) + cosr lat1 * cosr lat2 * square (sin (dlon))
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in (_Sphere # s) * c

-- | Haversine algorithm with a default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (haversineD fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (haversineD fr to)) :: Maybe String
-- Just "17128743.0669"
{-
haversineD ::
  (HasCoordinate c1, HasCoordinate c2) =>
  c1 -- ^ start coordinate
  -> c2 -- ^ end coordinate
  -> Double
  -}
haversineD =
  undef -- haversine earthMean

undef = undef  

-- | Haversine algorithm with an optionally applied default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (haversine' fr to :: Double)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (haversine' fr to :: Double)) :: Maybe String
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
