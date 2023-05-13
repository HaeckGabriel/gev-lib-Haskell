module Gev ( Distribution(..) ) where

import System.Random.Stateful (StatefulGen, uniformDouble01M)

-- | Distribution Class for the GEV family of distributions. That is, each of the 
-- distributions considered will have a CDF, PDF and Quantile function.
class Distribution d where
    -- | Cumulative Distribution Function (CDF) of a given distribution.
    -- i.e. $\mathbb{P}(X \leq x)$ for $x \in \Omega(X)$ (i.e. x is in the support of X)
    --
    -- > cdf d +∞ = 1
    -- > cdf d -∞ = 0
    cdf :: d -> Double -> Double
    cdf d x = 1 - complCdf d x

    -- | Complement of the CDF, i.e. $\mathbb{P}(X \geq x)$.
    complCdf :: d -> Double -> Double
    complCdf d x = 1 - cdf d x

    -- | Probability Density Function (pdf) of a distribution.
    -- i.e. $\mathbb{P}(X = x)$  for $x \in \Omega(X)$ (i.e. x is in the support of X)
    pdf :: d -> Double -> Double
    pdf d = exp . logPdf d
    
    -- | Log density of a given distribution
    -- i.e. density for $Y = \log X$
    logPdf :: d -> Double -> Double
    logPdf d = log . pdf d

    -- | Quantile function (a.k.a inverse CDF) of a distribution.
    -- i.e. $F^{-1}(x)$ for $x \in [0, 1]$.
    quantile :: d -> Double -> Double
    quantile d x = complQuantile d (1- x)

    -- | Quantile complement, i.e. Quantile for level $1 - \alpha$.
    complQuantile :: d -> Double -> Double
    complQuantile d x = quantile d (1 - x)

    -- | generate random value of the Distribution.
    randGen :: StatefulGen g m => d -> g -> m Double
    randGen d gen = do
        x <- uniformDouble01M gen
        return $! quantile d x
    {-# MINIMAL (cdf | complCdf), (pdf | logPdf), (quantile | complQuantile) #-}

-- see https://hackage.haskell.org/package/statistics-0.16.1.2/docs/src/Statistics.Distribution.html#genContinuous
--genContinuous :: (ContDistr d, StatefulGen g m) => d -> g -> m Double
--genContinuous d gen = do
--  x <- uniformDouble01M gen
--  return $! quantile d x
