{-# LANGUAGE FlexibleContexts #-}

-- | Great circle geodetic distance algorithm.
module Data.Geo.Geodetic.GreatCircle(
  sphericalLaw
, sphericalLawD
, sphericalLaw'
) where

import Prelude(Double, Num(..), Fractional(..), pi, sin, cos, acos)
import Control.Lens((#), (^.))
import System.Args.Optional(Optional1(..))
import Data.Geo.Coordinate
import Data.Geo.Geodetic.Sphere

-- $setup
-- >>> import Prelude(Functor(..), Monad(..), String)
-- >>> import Data.Maybe(Maybe)
-- >>> import Text.Printf(printf)

-- | Great circle spherical law algorithm.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (sphericalLaw earthMean fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (sphericalLaw earthMean fr to)) :: Maybe String
-- Just "17128743.0669"
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (sphericalLaw (6350000 ^. nSphere) fr to)) :: Maybe String
-- Just "14959840.4461"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (sphericalLaw (6350000 ^. nSphere) fr to)) :: Maybe String
-- Just "17081801.7377"
sphericalLaw ::
  (HasCoordinate c1, HasCoordinate c2) =>
  Sphere -- ^ reference sphere
  -> c1 -- ^ start coordinate
  -> c2 -- ^ end coordinate
  -> Double
sphericalLaw s start' end' =
  let start = start' ^. coordinate
      end = end' ^. coordinate
      toRadians n = n * pi / 180
      lat1 = toRadians (fracLatitude # (start ^. latitude))
      lat2 = toRadians (fracLatitude # (end ^. latitude))
      lon1 = toRadians (fracLongitude # (start ^. longitude))
      lon2 = toRadians (fracLongitude # (end ^. longitude))
  in acos (sin lat1 * sin lat2 + cos lat1 * cos lat2 * cos (lon2 - lon1)) * nSphere # s

-- | Great circle spherical law algorithm with a default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (sphericalLawD fr to)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (sphericalLawD fr to)) :: Maybe String
-- Just "17128743.0669"
sphericalLawD ::
  (HasCoordinate c1, HasCoordinate c2) =>
  c1 -- ^ start coordinate
  -> c2 -- ^ end coordinate
  -> Double
sphericalLawD =
  sphericalLaw earthMean

-- | Great circle spherical law algorithm with an optionally applied default sphere of the earth mean.
--
-- >>> fmap (printf "%0.4f") (do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (sphericalLaw' fr to :: Double)) :: Maybe String
-- Just "15000950.5589"
--
-- >>> fmap (printf "%0.4f") (do fr <- (-16.7889) ..#.. 41.935; to <- 6.933 ..#.. (-162.55); return (sphericalLaw' fr to :: Double)) :: Maybe String
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
