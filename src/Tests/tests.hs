-- | tests for the library

module Main where

import Gev
import Gev.Gumbel
import Gev.Frechet
import Gev.Weibull
import Gev.GevDist

import Test.HUnit
import qualified System.Exit as Exit

------------------------------------ Gumbel ------------------------------------

-- | Gumbel dist instance.
gumbel :: GumbelDistribution
gumbel = gumbelDist 0.5 2.0

-- | individual test
testGumbelCDF :: Test
testGumbelCDF = TestCase (assertEqual "Gumbel CDF test" 0.6235249162568004 (cdf gumbel 2.0))

testGumbelPDF :: Test
testGumbelPDF = TestCase (assertEqual "Gumbel PDF test" 0.14726615762017733 (pdf gumbel 2.0))

testGumbelQuant :: Test
testGumbelQuant = TestCase (assertEqual "Gumbel Qauntile test" 2.5618608663174456 (quantile gumbel 0.7))

------------------------------------ Frechet -----------------------------------

-- | Frechet dist instance.
frechet :: FrechetDistribution
frechet = frechetDist 1.0 0.1 1.0

-- | individual test
testFrechetCDF :: Test
testFrechetCDF = TestCase (assertEqual "Frechet CDF test" 0.951229424500714 (cdf frechet 3.0))

testFrechetPDF :: Test
testFrechetPDF = TestCase (assertEqual "Frechet PDF test" 0.023780735612517853 (pdf frechet 3.0))

testFrechetQuant :: Test
testFrechetQuant = TestCase (assertEqual "Frechet Qauntile test" 1.2803673252057128 (quantile frechet 0.7))

------------------------------------ Weibull -----------------------------------

-- | Weibull dist instance.
weibull :: WeibullDistribution
weibull = weibullDist 2.0 2.0 2.0

-- | individual test
testWeibullCDF :: Test
testWeibullCDF = TestCase (assertEqual "Weibull CDF test" 0.7788007830714049 (cdf weibull 1.0))

testWeibullPDF :: Test
testWeibullPDF = TestCase (assertEqual "Weibull PDF test" 0.38940039153570244 (pdf weibull 1.0))

testWeibullQuant :: Test
testWeibullQuant = TestCase (assertEqual "Weibull Qauntile test" 0.8055546158342233 (quantile weibull 0.7))


------------------------------------ GEV -----------------------------------

--------- GEV with shape not equal to 0

-- | GEV dist instance.
gev :: GevDistribution
gev = gevDist 2.0 2.0 2.0

-- | individual test
testGEVCDF :: Test
testGEVCDF = TestCase (assertEqual "GEV CDF test" 0.4930686913952398 (cdf gev 3.0))

testGEVPDF :: Test
testGEVPDF = TestCase (assertEqual "GEV PDF test" 0.0871630538190878 (pdf gev 3.0))

testGEVQuant :: Test
testGEVQuant = TestCase (assertEqual "GEV Qauntile test" 8.860583704300595 (quantile gev 0.7))


--------- GEV with shape equal to 0

-- | GEV dist instance with shape equal to 0.
gevAlt :: GevDistribution
gevAlt = gevDist 2.0 2.0 0.0

-- | individual test
testGEVCDFAlt :: Test
testGEVCDFAlt = TestCase (assertEqual "GEV CDF test" 0.545239211892605 (cdf gevAlt 3.0))

testGEVPDFAlt :: Test
testGEVPDFAlt = TestCase (assertEqual "GEV PDF test" 0.16535214944520904 (pdf gevAlt 3.0))

testGEVQuantAlt :: Test
testGEVQuantAlt = TestCase (assertEqual "GEV Qauntile test" 4.061860866317446 (quantile gevAlt 0.7))

-- | list of tests
testList :: Test
testList = TestList [ TestLabel "testGumbelCDF" testGumbelCDF
                    , TestLabel "testGumbelPDF" testGumbelPDF
                    , TestLabel "testGumbelQuant" testGumbelQuant
                    , TestLabel "testFrechetCDF" testFrechetCDF
                    , TestLabel "testFrechetPDF" testFrechetPDF
                    , TestLabel "testFrechetQuant" testFrechetQuant
                    , TestLabel "testWeibullCDF" testWeibullCDF
                    , TestLabel "testWeibullPDF" testWeibullPDF
                    , TestLabel "testWeibullQuant" testWeibullQuant
                    , TestLabel "testGEVCDF" testGEVCDF
                    , TestLabel "testGEVPDF" testGEVPDF
                    , TestLabel "testGEVQuant" testGEVQuant
                    , TestLabel "testGEVCDFAlt" testGEVCDFAlt
                    , TestLabel "testGEVPDFAlt" testGEVPDFAlt
                    , TestLabel "testGEVQuantAlt" testGEVQuantAlt]

main :: IO ()
main = do
    result <- runTestTT testList
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
