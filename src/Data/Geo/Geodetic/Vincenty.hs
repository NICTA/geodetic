{-# LANGUAGE FlexibleContexts #-}

-- | An implementation of Thaddeus Vincenty's direct and inverse geodetic algorithms. <http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf>
module Data.Geo.Geodetic.Vincenty(
  Convergence
, convergence
, direct
, directD
, direct'
, VincentyDirectResult
, inverse
, inverseD
, inverse'
) where

import Prelude(Eq(..), Show(..), Ord(..), Num(..), Floating(..), Fractional(..), Double, Int, Bool, Ordering(..), subtract, cos, sin, asin, tan, sqrt, atan, atan2, pi, (.), (++), (&&), ($!), error)
import Control.Lens(lens, (^.), (#), (^?))
import Data.Maybe(fromMaybe)
import System.Args.Optional(Optional2(..))
import Data.Geo.Coordinate
import Data.Geo.Geodetic.Azimuth
import Data.Geo.Geodetic.Bearing
import Data.Geo.Geodetic.Ellipsoid
import Data.Geo.Geodetic.Curve

-- $setup
-- >>> import Prelude

type Convergence =
  Double

-- | A typically acceptable convergence value.
convergence ::
  Convergence
convergence =
  0.0000000000001

data VincentyDirectResult =
  VincentyDirectResult
    Coordinate
    Bearing
  deriving (Eq, Ord, Show)

instance HasCoordinate VincentyDirectResult where
  coordinate =
    lens (\(VincentyDirectResult c _) -> c) (\(VincentyDirectResult _ b) c -> VincentyDirectResult c b)

instance HasBearing VincentyDirectResult where
  bearing =
    lens (\(VincentyDirectResult _ b) -> b) (\(VincentyDirectResult c _) b -> VincentyDirectResult c b)

-- | Vincenty direct algorithm.
--
-- >>> fmap (\c' -> direct wgs84 convergence c' (modBearing 165.34) 4235) (27.812 ..#.. 154.295)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude 27) (Minutes 46) (Seconds 30.0981)) (Longitude (DegreesLongitude 154) (Minutes 18) (Seconds 21.1466))) (Bearing 165.3451))
--
-- >>> fmap (\c' -> direct wgs84 convergence c' (modBearing 165.34) 4235) ((-66.093) ..#.. 12.84)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude (-66)) (Minutes 7) (Seconds 47.0667)) (Longitude (DegreesLongitude 12) (Minutes 51) (Seconds 49.4142))) (Bearing 165.3183))
--
-- >>> fmap (\c' -> direct ans convergence c' (modBearing 165.34) 4235) (27.812 ..#.. 154.295)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude 27) (Minutes 46) (Seconds 30.0986)) (Longitude (DegreesLongitude 154) (Minutes 18) (Seconds 21.1464))) (Bearing 165.3451))
--
-- >>> fmap (\c' -> direct ans convergence c' (modBearing 165.34) 4235) ((-66.093) ..#.. 12.84)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude (-66)) (Minutes 7) (Seconds 47.0662)) (Longitude (DegreesLongitude 12) (Minutes 51) (Seconds 49.4139))) (Bearing 165.3183))
direct ::
  (HasEllipsoid e, HasCoordinate c, HasBearing b) =>
  e -- ^ reference ellipsoid
  -> Convergence -- ^ convergence point to stop calculating
  -> c -- ^ begin coordinate
  -> b -- ^ bearing
  -> Double -- ^ distance
  -> VincentyDirectResult
direct e' conv start' bear' dist =
  let e = e' ^. ellipsoid
      start = start' ^. coordinate
      bear = bear' ^. bearing
      sMnr = e ^. semiMinor
      flat = e ^. flattening
      alpha = radianBearing # bear
      cosAlpha = cos alpha
      sinAlpha = sin alpha
      tanu1 = (1.0 - flat) * tan (radianLatitude # (start ^. latitude))
      cosu1 = 1.0 / sqrt (1.0 + square tanu1)
      sinu1 = tanu1 * cosu1
      sigma1 = atan2 tanu1 cosAlpha
      csa = cosu1 * sinAlpha
      sin2Alpha = square csa
      cos2Alpha = 1 - sin2Alpha
      ab d f g h i = let s = cos2Alpha * (square (e ^. semiMajor / sMnr) - 1)
                     in (s / d) * (f + s * (g + s * (h - i * s)))
      a = 1 + ab 16384 4096 (-768) 320 175
      b = ab 1024 256 (-128) 74 47
      end = let begin = ps (dist / sMnr / a)
                iter p = let tf d = -3 + 4 * d
                             cosSigma'' = cosSigma' p
                             sinSigma'' = sinSigma' p
                             cosSigmaM2'' = cosSigmaM2' sigma1 p
                             cos2SigmaM2'' = cos2SigmaM2' sigma1 p
                             deltaSigma = b * sinSigma'' * (cosSigmaM2'' + b / 4.0 * (cosSigma'' * (-1 + 2 * cos2SigmaM2'') - (b / 6.0) * cosSigmaM2'' * tf (square sinSigma'') * tf cos2SigmaM2''))
                         in transition p deltaSigma
                pred' p = abs (sigma' p - prevSigma' p) >= conv
            in doWhile iter pred' begin
      sigma'' = sigma' end
      sinSigma = sinSigma' end
      cosSigmaM2 = cosSigmaM2' sigma1 end
      cos2SigmaM2 = cos2SigmaM2' sigma1 end
      cosSigma = cos sigma''
      c = flat / 16 * cos2Alpha * (4 + flat * (4 - 3 * cos2Alpha))
      cc = cosu1 * cosSigma
      ccca = cc * cosAlpha
      sss = sinu1 * sinSigma
      latitude' = let r = atan2 (sinu1 * cosSigma + cosu1 * sinSigma * cosAlpha) ((1.0 - flat) * sqrt (sin2Alpha + (sss - ccca) ** 2.0))
                  in fromMaybe (error ("Invariant not met. Latitude in radians not within range " ++ show r)) (r ^? radianLatitude)
      longitude' = let r = fracLongitude # (start ^. longitude) + ((atan2 (sinSigma * sinAlpha) (cc - sss * cosAlpha) - (1 - c) * flat * csa * (sigma'' + c * sinSigma * (cosSigmaM2 + c * cosSigma * (-1 + 2 * cos2SigmaM2)))) * 180 / pi)
                    in fromMaybe (error ("Invariant not met. Longitude in radians not within range " ++ show r)) (r ^? fracLongitude)
  in VincentyDirectResult
       (latitude' .#. longitude')
       (
         let r = atan2 csa (ccca - sss)
         in fromMaybe (error ("Invariant not met. Bearing in radians not within range " ++ show r)) (r ^? radianBearing)
       )

-- | Vincenty direct algorithm with a default ellipsoid of WGS84 and standard convergence.
--
-- >>> fmap (\c' -> directD c' (modBearing 165.34) 4235) (27.812 ..#.. 154.295)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude 27) (Minutes 46) (Seconds 30.0981)) (Longitude (DegreesLongitude 154) (Minutes 18) (Seconds 21.1466))) (Bearing 165.3451))
--
-- >>> fmap (\c' -> directD c' (modBearing 165.34) 4235) ((-66.093) ..#.. 12.84)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude (-66)) (Minutes 7) (Seconds 47.0667)) (Longitude (DegreesLongitude 12) (Minutes 51) (Seconds 49.4142))) (Bearing 165.3183))
directD ::
  (HasCoordinate c, HasBearing b) =>
  c -- ^ begin coordinate
  -> b -- ^ bearing
  -> Double -- ^ distance
  -> VincentyDirectResult
directD =
  direct wgs84 convergence

-- | Vincenty direct algorithm with an optionally applied default ellipsoid of WGS84 and standard convergence.
--
-- >>> fmap (\c' -> direct' c' (modBearing 165.34) (4235 :: Double) :: VincentyDirectResult) (27.812 ..#.. 154.295)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude 27) (Minutes 46) (Seconds 30.0981)) (Longitude (DegreesLongitude 154) (Minutes 18) (Seconds 21.1466))) (Bearing 165.3451))
--
-- >>> fmap (\c' -> direct' c' (modBearing 165.34) (4235 :: Double) :: VincentyDirectResult) ((-66.093) ..#.. 12.84)
-- Just (VincentyDirectResult (Coordinate (Latitude (DegreesLatitude (-66)) (Minutes 7) (Seconds 47.0667)) (Longitude (DegreesLongitude 12) (Minutes 51) (Seconds 49.4142))) (Bearing 165.3183))
direct' ::
  (Optional2
    Ellipsoid
    Convergence
    (
      Coordinate
      -> Bearing
      -> Double
      -> VincentyDirectResult
    ) x) =>
    x
direct' =
  optional2 (direct :: Ellipsoid -> Convergence -> Coordinate -> Bearing -> Double -> VincentyDirectResult) wgs84 convergence

-- | Vincenty inverse algorithm.
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (inverse wgs84 convergence fr to)
-- Just (GeodeticCurve 14998576.9860 Azimuth 180.0000 Azimuth 0.0000)
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- 87.7769 ..#.. 19.944; return (inverse wgs84 convergence fr to)
-- Just (GeodeticCurve 7099204.2589 Azimuth 0.0000 Azimuth 180.0000)
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (inverse ans convergence fr to)
-- Just (GeodeticCurve 14998630.4056 Azimuth 180.0000 Azimuth 0.0000)
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- 87.7769 ..#.. 19.944; return (inverse ans convergence fr to)
-- Just (GeodeticCurve 7099229.9126 Azimuth 0.0000 Azimuth 180.0000)
inverse ::
  (HasEllipsoid e, HasCoordinate c1, HasCoordinate c2) =>
  e -- ^ reference ellipsoid
  -> Convergence -- ^ convergence point to stop calculating
  -> c1 -- ^ start coordinate
  -> c2 -- ^ end coordinate
  -> Curve
inverse e' conv start' end' =
  let e = e' ^. ellipsoid
      start = start' ^. coordinate
      end = end' ^. coordinate
      b = e ^. semiMinor
      f = e ^. flattening
      (phi1, phi2) =
        let rl k = radianLatitude # (k ^. latitude)
        in (rl start, rl end)
      a2b2b2 =
        let ss z = square (z e)
        in ss (^. semiMajor) / ss (^. semiMinor) - 1
      omega =
        let rl k = radianLongitude # (k ^. longitude)
        in rl end - rl start
      (u1, u2) =
        let at = atan . ((1 - f) *) . tan
        in (at phi1, at phi2)
      sinu1 = sin u1
      cosu1 = cos u1
      sinu2 = sin u2
      cosu2 = cos u2
      sinu1sinu2 = sinu1 * sinu2
      cosu1sinu2 = cosu1 * sinu2
      sinu1cosu2 = sinu1 * cosu2
      cosu1cosu2 = cosu1 * cosu2
      begin = Q 0 Continue omega 0 0 0
      iter q = let sinlambda = sin (lambda q)
                   coslambda = cos (lambda q)
                   sin2sigma = square cosu2 * square sinlambda + square (cosu1sinu2 - sinu1cosu2 * coslambda)
                   sinsigma = sqrt sin2sigma
                   cossigma = sinu1sinu2 + cosu1cosu2 * coslambda
                   sigma'' = atan2 sinsigma cossigma
                   sinalpha = if sin2sigma == 0.0 then 0.0 else cosu1cosu2 * sinlambda / sinsigma
                   alpha = asin sinalpha
                   cos2alpha = square (cos alpha)
                   cos2sigmam = if cos2alpha == 0.0 then 0.0 else cossigma - 2 * sinu1sinu2 / cos2alpha
                   u2' = cos2alpha * a2b2b2
                   cos2sigmam2 = square cos2sigmam
                   a = 1.0 + u2' / 16384 * (4096 + u2' * (u2' * (320 - 175 * u2') - 768))
                   b' = u2' / 1024 * (256 + u2' * (u2' * (74 - 47 * u2') - 128))
                   deltasigma' = b' * sinsigma * (cos2sigmam + b' / 4 * (cossigma * (2 * cos2sigmam2 - 1) - b' / 6 * cos2sigmam * (4 * sin2sigma - 3) * (cos2sigmam2 * 4 - 3)))
                   c' = f / 16 * cos2alpha * (4 + f * (4 - 3 * cos2alpha))
                   l = omega + (1 - c') * f * sinalpha * (sigma'' + c' * sinsigma * (cos2sigmam + c' * cossigma * (2 * cos2sigmam2 - 1)))
                   r = let c = count q
                       in if c == 20
                             then Limit
                             else if c > 1 && cos alpha < conv
                                   then Converge
                                   else Continue
              in Q (count q + 1) r l a sigma'' deltasigma'
      pred' = (== Continue) . result
      ed = whileDo iter pred' begin
      ifi p t a = if p a then t a else a
      (alpha1, alpha2) =
        let alphaNoConverge c cp x y =
              vmap2 (ifi (>= 360) (subtract 360)) (if c
                                                     then (x, y)
                                                     else if cp == GT
                                                       then (180.0, 0.0)
                                                       else if cp == LT
                                                         then (0.0, 180.0)
                                                         else let nan = 0/0
                                                               in (nan, nan))
        in alphaNoConverge (result ed == Converge) (compare phi1 phi2) 0 0
  in curve (b * a' ed * (sigma ed - deltasigma ed)) (modAzimuth alpha1) (modAzimuth alpha2)

-- | Vincenty inverse algorithm with a default ellipsoid of WGS84 and standard convergence.
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (inverseD fr to)
-- Just (GeodeticCurve 14998576.9860 Azimuth 180.0000 Azimuth 0.0000)
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- 87.7769 ..#.. 19.944; return (inverseD fr to)
-- Just (GeodeticCurve 7099204.2589 Azimuth 0.0000 Azimuth 180.0000)
inverseD ::
  (HasCoordinate c1, HasCoordinate c2) =>
  c1 -- ^ start coordinate
  -> c2 -- ^ end coordinate
  -> Curve
inverseD =
  inverse wgs84 convergence

-- | Vincenty inverse algorithm with an optionally applied default ellipsoid of WGS84 and standard convergence.
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- (-66.093) ..#.. 12.84; return (inverse' fr to :: Curve)
-- Just (GeodeticCurve 14998576.9860 Azimuth 180.0000 Azimuth 0.0000)
--
-- >>> do fr <- 27.812 ..#.. 154.295; to <- 87.7769 ..#.. 19.944; return (inverse' fr to :: Curve)
-- Just (GeodeticCurve 7099204.2589 Azimuth 0.0000 Azimuth 180.0000)
inverse' ::
  (Optional2
    Ellipsoid
    Convergence
    (
      Coordinate
      -> Coordinate
      -> Curve
    ) x) =>
    x
inverse' =
  optional2 (inverse :: Ellipsoid -> Convergence -> Coordinate -> Coordinate -> Curve) wgs84 convergence

---- not exported

data P = P {
  origSigma' :: Double
, sigma' :: Double
, prevSigma' :: Double
} deriving Show

vmap2 ::
  (a -> b)
  -> (a, a)
  -> (b, b)
vmap2 f (a1, a2) =
  (f a1, f a2)

ps ::
  Double
  -> P
ps s =
  P s s s

transition ::
  P
  -> Double
  -> P
transition p d =
  P (origSigma' p) (d + origSigma' p) (sigma' p)

sinSigma' ::
  P
  -> Double
sinSigma' =
  sin . sigma'

cosSigma' ::
  P
  -> Double
cosSigma' =
  cos . sigma'

sigmaM2' ::
  Double
  -> P
  -> Double
sigmaM2' s p =
  2.0 * s + sigma' p

cosSigmaM2' ::
  Double
  -> P
  -> Double
cosSigmaM2' s p =
  cos (sigmaM2' s p)

cos2SigmaM2' ::
  Double
  -> P
  -> Double
cos2SigmaM2' s p =
  square (cosSigmaM2' s p)

square ::
  Num a =>
  a
  -> a
square a =
  a * a

doWhile ::
  (a -> a)
  -> (a -> Bool)
  -> a
  -> a
doWhile f p a =
  let x = f a
  in if p x then doWhile f p x else x

whileDo :: (a -> a) -> (a -> Bool) -> a -> a
whileDo f p a = if p a then whileDo f p $! (f a) else a

data InverseResult = Continue | Limit | Converge deriving Eq

data Q = Q {
  count :: Int,
  result :: InverseResult,
  lambda :: Double,
  a' :: Double,
  sigma :: Double,
  deltasigma :: Double
}