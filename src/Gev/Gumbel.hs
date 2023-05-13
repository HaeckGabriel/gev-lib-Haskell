module Gev.Gumbel
    (
      GumbelDistribution
    -- * Constructors
    , gumbelDist
    , gumbelDistMaybe
    -- * Accessors
    , location
    , scale
    ) where

import qualified Gev
-- import Numeric.SpecFunctions (log1p,expm1) POSSIBLY!!!

data GumbelDistribution = Gumbel {
      location :: {-# UNPACK #-} !Double
    , scale    :: {-# UNPACK #-} !Double
    } deriving (Eq)

instance Show GumbelDistribution where
    show (Gumbel loc sc) = show "Gumbel Distribution; loc: " ++ show loc ++ " and scale: " ++ show sc

-- error message when initiating Gumbel with scale parameter less than 0.
gumbelErrMsg :: Double -> Double -> String
gumbelErrMsg loc scale = "Gev.Gumbel: " 
    ++ "loc = " ++ show loc
    ++ " scale = " ++ show scale
    ++ ", but the scale parameter must be positive!"

-- | create Gumbel Dist, where scale parameter must be greater than 0.
gumbelDistMaybe :: Double -> Double -> Maybe GumbelDistribution
gumbelDistMaybe loc sc
    | sc > 0    = Just $ Gumbel loc sc
    | otherwise = Nothing

-- | create Gumbel Dist, where scale parameter must be greater than 0.
gumbelDist :: Double -> Double -> GumbelDistribution
gumbelDist loc sc = maybe (error $ gumbelErrMsg loc sc) id $ gumbelDistMaybe loc sc

-- | The CDF of the Gumbel distribution
cdfGumbel :: GumbelDistribution -> Double -> Double
cdfGumbel (Gumbel loc sc) x
    | x <= 0     = 0
    | otherwise  = exp $ - (exp y)
        where 
            y = - (x - loc) / sc

-- | The PDf of the Gumbel distribution
pdfGumbel :: GumbelDistribution -> Double -> Double
pdfGumbel (Gumbel loc sc) x =
    let exp_y = exp $ - (x- loc) / sc
        const = 1 / sc
    in const * exp_y * (exp $ - exp_y)

quantileGumbel :: GumbelDistribution -> Double -> Double
quantileGumbel (Gumbel loc sc) x
    | x == 0         = -inf
    | x == 1         = inf
    | x > 0 && x < 1 = loc - sc * (log $ - log x)
    | otherwise      =
        error $ "Gev.GumbelDistribution.quantile: The given value must be between 0 and 1, got: " ++ show x
    where inf = 1 / 0

-- | Gev.Distribution instance implementation for the Gumbel Distribution
instance Gev.Distribution GumbelDistribution where
    cdf      = cdfGumbel
    pdf      = pdfGumbel
    quantile = quantileGumbel
