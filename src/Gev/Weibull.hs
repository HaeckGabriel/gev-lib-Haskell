module Gev.Weibull
    (
      WeibullDistribution
     -- * constructors
    , weibullDist
    , weibullDistMaybe
     -- * accessors
    , location
    , scale
    , shape
    ) where

import qualified Gev

data WeibullDistribution = Weibull {
      location :: {-# UNPACK #-} !Double
    , scale    :: {-# UNPACK #-} !Double
    , shape    :: {-# UNPACK #-} !Double
    } deriving (Eq)

instance Show WeibullDistribution where
    show (Weibull loc sc sh) = show "Weibull Distribution; loc: " ++ show loc ++ ", scale: " ++ show sc ++ " and shape: " ++ show sh

-- error message when initiating Weibull with scale and/or shape parameter less than 0.
weibullErrMsg :: Double -> Double -> Double -> String
weibullErrMsg loc scale sh = "Gev.Weibull: " 
    ++ "loc = " ++ show loc
    ++ " scale = " ++ show scale
    ++ " schape = " ++ show sh
    ++ ", but both the scale and shape parameters must be positive!"

weibullDistMaybe :: Double -> Double -> Double -> Maybe WeibullDistribution
weibullDistMaybe loc sc sh
    | sc > 0 && sh > 0 = Just $ Weibull loc sc sh
    | otherwise = Nothing

weibullDist :: Double -> Double -> Double -> WeibullDistribution
weibullDist loc sc sh = maybe (error $ weibullErrMsg loc sc sh) id $ weibullDistMaybe loc sh sc

-- | The CDF of the Weibull distribution
cdfWeibull :: WeibullDistribution -> Double -> Double
cdfWeibull (Weibull loc sc sh) x
    | x <= 0     = 0
    | otherwise  = exp $ - ((- y) ** sh)
        where 
            y   = (x - loc) / sc

-- | The PDF of the Weibull distribution
pdfWeibull :: WeibullDistribution -> Double -> Double
pdfWeibull (Weibull loc sc sh) x =
    let y       = (x - loc) / sc
        const   = sh / sc
        expterm = exp $ - ((- y) ** sh)
        middle  = (- y) ** (sh - 1)
    in const * middle * expterm

quantileWeibull :: WeibullDistribution -> Double -> Double
quantileWeibull (Weibull loc sc sh) x
    | x == 0          = -inf
    | x == 1          = inf
    | x > 0 && x < 1  = loc - sc * (logexp ** pow)
    | otherwise       =
        error $ "Gev.WeibullDistribution.quantile: The given value must be between 0 and 1, got: " ++ show x
    where
        inf    = 1 / 0
        logexp = - log x
        pow    = 1 / sh

instance Gev.Distribution WeibullDistribution where
    pdf      = pdfWeibull
    cdf      = cdfWeibull
    quantile = quantileWeibull
