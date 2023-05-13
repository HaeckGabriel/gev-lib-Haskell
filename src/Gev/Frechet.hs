module Gev.Frechet
    (
      FrechetDistribution
     -- * constructors
    , frechetDist
    , frechetDistMaybe
     -- * accessors
    , location
    , scale
    , shape
    ) where

import qualified Gev

data FrechetDistribution = Frechet {
      location :: {-# UNPACK #-} !Double
    , scale    :: {-# UNPACK #-} !Double
    , shape    :: {-# UNPACK #-} !Double
    } deriving (Eq)

instance Show FrechetDistribution where
    show (Frechet loc sc sh) = show "Frechet Distribution; loc: " ++ show loc ++ ", scale: " ++ show sc ++ " and shape: " ++ show sh

-- error message when initiating Frechet with scale parameter less than 0.
frechetErrMsg :: Double -> Double -> Double -> String
frechetErrMsg loc scale sh = "Gev.Frechet: " 
    ++ "loc = " ++ show loc
    ++ " scale = " ++ show scale
    ++ " schape = " ++ show sh
    ++ ", but both the scale and shape parameters must be positive!"

-- | create Frechet Dist, where scale parameter must be greater than 0.
frechetDistMaybe :: Double -> Double -> Double -> Maybe FrechetDistribution
frechetDistMaybe loc sc sh
    | sc > 0 && sh > 0    = Just $ Frechet loc sc sh
    | otherwise = Nothing

-- | create Frechet Dist, where scale parameter must be greater than 0.
frechetDist :: Double -> Double -> Double -> FrechetDistribution
frechetDist loc sc sh = maybe (error $ frechetErrMsg loc sc sh) id $ frechetDistMaybe loc sc sh

-- | The CDF of the Frechet distribution
cdfFrechet :: FrechetDistribution -> Double -> Double
cdfFrechet (Frechet loc sc sh) x
    | x <= 0     = 0
    | otherwise  = exp $ - (y ** pow)
        where 
            y   = (x - loc) / sc
            pow = - sh

-- | The PDF of the Frechet distribution
pdfFrechet :: FrechetDistribution -> Double -> Double
pdfFrechet (Frechet loc sc sh) x =
    let y       = (x - loc) / sc
        const   = sh / sc
        expterm = exp $ - (y ** (- sh))
        middle  = y ** (-1 - sh)
    in const * middle * expterm

-- Quantile function of the Frechet Distribution
quantileFrechet :: FrechetDistribution -> Double -> Double
quantileFrechet (Frechet loc sc sh) x
    | x == 0         = -inf
    | x == 1         = inf
    | x > 0 && x < 1 = loc + sc * (logexp ** pow)
    | otherwise      =
        error $ "Gev.FrechetDistribution.quantile: The given value must be between 0 and 1, got: " ++ show x
    where 
        inf    = 1 / 0
        logexp = - log x
        pow    = - 1 / sh

-- | Gev.Distribution instance implementation for the Frechet Distribution
instance Gev.Distribution FrechetDistribution where
    cdf      = cdfFrechet
    pdf      = pdfFrechet
    quantile = quantileFrechet
