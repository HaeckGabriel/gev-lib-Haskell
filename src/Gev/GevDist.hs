module Gev.GevDist 
    (
     GevDistribution
     -- * constructors
    , gevDist
    , gevDistMaybe
     -- * accessors
    , location
    , scale
    , shape
    ) where

import qualified Gev

data GevDistribution = GEV {
      location :: {-# UNPACK #-} !Double
    , scale    :: {-# UNPACK #-} !Double
    , shape    :: {-# UNPACK #-} !Double
    } deriving (Eq)

instance Show GevDistribution where
    show (GEV loc sc sh) = show "GEV Distribution; loc: " ++ show loc ++ ", scale: " ++ show sc ++ " and shape: " ++ show sh

-- error message when initiating GEV Distribution with scale parameter less than 0.
gevErrMsg :: Double -> Double -> Double -> String
gevErrMsg loc scale sh = "Gev.GevDist: " 
    ++ "loc = " ++ show loc
    ++ " scale = " ++ show scale
    ++ " schape = " ++ show sh
    ++ ", but the scale parameter must be positive!"

gevDistMaybe :: Double -> Double -> Double -> Maybe GevDistribution
gevDistMaybe loc sc sh
    | sc > 0 = Just $ GEV loc sc sh
    | otherwise = Nothing

gevDist :: Double -> Double -> Double -> GevDistribution
gevDist loc sc sh = maybe (error $ gevErrMsg loc sc sh) id $ gevDistMaybe loc sh sc

-- | helper function: checks that the given x value falls in the valid part of the GEV domain.
-- -- actually this i isn't needed, just included as a guard pattern it'll be fine..
gevDomainCheck :: GevDistribution -> Double -> Bool
gevDomainCheck (GEV loc sc sh) x =
    if 1 + sh * (x - loc / sc) > 0 then True else False

-- | t(x) function depends on if the shape parameter (\zeta) is 0 or not.
-- t(x) = \exp \left(x) = \left( 1 + \zeta \left( \frac{x - \mu}{ \sigma} \right) \right)^{- \frac{1}{\zeta}}$$ if $\zeta \neq 0$,
--  or $t(x) = \exp \left \{ - \frac{x - \mu}{ \sigma }  \right \}$ if $\zeta = 0$
gevArg :: Double -> Double -> Double -> Double -> Double
gevArg loc sc sh x =
    if sh == 0 then one else two
        where
            y   = (x - loc) / sc
            one = exp $ - y
            two = (1 + sh * y) ** (- 1 / sh)
   
-- | CDF of the GEV Distribution.
cdfGEV :: GevDistribution -> Double -> Double
cdfGEV (GEV loc sc sh) x
   | x <= 0                       = 0
   | 1 + sh * (x - loc / sc) > 0  = exp $ - tVal
   | otherwise =
        error $ "Gev.GevDist.cdf: The given x value is not in the support of the Distribution: " ++ show x
   where
       tVal = gevArg loc sc sh x
        

-- | PDF of the GEV Distribution.
pdfGEV :: GevDistribution -> Double -> Double
pdfGEV (GEV loc sc sh) x
   | 1 + sh * (x - loc / sc) > 0  = const * middle * exp ( - tVal)
   | otherwise =
        error $ "Gev.GevDist.pdf: The given x value is not in the support of the Distribution: " ++ show x
   where
       tVal   = gevArg loc sc sh x
       const  = 1 / sc
       middle = tVal ** (sh + 1)

-- | Quantile function of the GEV Distribution
-- Quantile function of the Frechet Distribution
quantileGEV :: GevDistribution -> Double -> Double
quantileGEV (GEV loc sc sh) x
    | x > 0 && x < 1 = if sh == 0 then one else two
    | otherwise      =
        error $ "Gev.GEVDist.quantile: The given value must be between 0 and 1, got: " ++ show x
    where 
        logx  = - log x
        one   = - sc * log logx + loc
        const = sc / sh
        two   = const * (logx ** (- sh)) - const + loc

instance Gev.Distribution GevDistribution where
    cdf      = cdfGEV
    pdf      = pdfGEV
    quantile = quantileGEV
