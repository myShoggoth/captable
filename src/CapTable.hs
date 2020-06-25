{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module CapTable
(
  module CapTable
) where

import           CapTable.Types
import           Control.Lens
import           Data.List
import           Data.Generics.Product

-- import           Debug.Trace

-- | Price per share for a round.
pps :: Calculation -> Round -> Double
-- pps _ r | trace ("PPS for " ++ show r) False = undefined
pps _ (Round _ _ (PricePerShare p) _ _ _ _)                             = p
pps c r@(Round _ _ _ _ _ (OptionPool (OptionPoolPercent _) PreMoney) _) = (pmv c r) / (fromInteger $ tspmos c r)
pps c r                                                                 = (pmv c r) / (fromInteger $ ts c $ pr c r)

-- | The total pre-money option pool shares.
tspmos :: Calculation -> Round -> Integer
-- tspmos _ r | trace ("TS PMOS for " ++ show r) False = undefined
tspmos c r@(Round _ _ _ _ _ (OptionPool _ PreMoney) _) = floor $ prs * (pmo + opp r)
  where
    pmp = ti r / (pmv c r + ti r)
    pmo = 1.0 - pmp - opp r
    prs = ( fromInteger (ts c (pr c r)) ) / pmo
tspmos _ _ = error "Getting pre-money option pool shares for non-premoney option pool round."

-- | Pre-Money Valuation for a round.
pmv :: Calculation -> Round -> Double
-- pmv _ r | trace ("PMV for " ++ show r) False = undefined
pmv _ (Round _ _ (PreMoneyValuation p) _ _ _ _) = p
pmv c r@(Round _ _ (PricePerShare p) _ _ _ _)   = p * ( fromInteger ( (ts c $ pr c r) + ops c (Just r) ) )
pmv c r@(Round _ _ (PostEquityStake p) _ _ _ _) = ( (ti r) / ( ( 1.0 - p ) - fromInteger ( ts c (Just r))) ) * fromInteger ( ts c (pr c r) )

-- | Post-Money Valuation for a round.
postmv :: Calculation -> Round -> Double
postmv _ r@(Round _ _ (PreMoneyValuation p) _ _ _ _)  = ti r + p
postmv c r@(Round _ _ (PricePerShare _) _ _ _ _)      = ti r + pmv c r
postmv _ (Round _ _ (PostEquityStake _) _ _ _ _)      = error "Unable to calculate post-money value due to post-equity stake."


-- | Total Shares for a round.
--   In this case Nothing refers to the foundation of the company.
ts :: Calculation -> Maybe Round -> Integer
-- ts _ Nothing | trace "TS for Nothing" False = undefined
ts c Nothing  = foldr ((+) . shares 0) 0 (c ^. field @"company" . field @"founders")
-- ts _ (Just r) | trace ("TS for " ++ show r) False = undefined
ts c mr@(Just r) = foldr ((+) . shares (pps c r)) 0 (r ^. field @"investments") + (ts c $ pr c r) - (ops c (pr c r)) + (ops c mr)

-- | Total monetary amount invested in a round.
ti :: Round -> Double
-- ti r | trace ("TI for " ++ show r) False = undefined
ti r@(Round _ _ (PricePerShare p) _ _ _ _)   = foldr ((+) . invested (Just p)) 0 (r ^. field @"investments")
ti r = foldr ((+) . invested Nothing) 0 (r ^. field @"investments")

-- | The number of shares for an investment given a price per share.
shares :: Double -> Investment -> Integer
shares p (Investment _ (Currency a)) = floor (a / p)
shares _ (Investment _ (Shares s))   = s

-- | Total monetary amount invested in an investment.
invested :: Maybe Double -> Investment -> Double
invested _ (Investment _ (Currency a))      = a
invested (Just p) (Investment _ (Shares s)) = fromInteger s * p
invested _ _ = error "Unable to calculate amount for an investment due to lack of price per share"

-- | The previous round for a given round, or Nothing if foundation.
pr :: Calculation -> Round -> (Maybe Round)
pr c r = maybeIndex (r `elemIndex` rs)
  where
        rs = sort (c ^. field @"company" . field @"rounds")
        maybeIndex Nothing  = Nothing
        maybeIndex (Just 0) = Nothing
        maybeIndex (Just i) = Just $ rs !! (i - 1)

-- | The number of option pool shares for a round.
ops :: Calculation -> Maybe Round -> Integer
ops _ Nothing                                                           = 0
ops c (Just r@(Round _ _ _ _ _ (OptionPool (OptionPoolShares 0) _) _))  = ops c $ pr c r
ops _ (Just (Round _ _ _ _ _ (OptionPool (OptionPoolShares s) _) _))    = s
ops c (Just r@(Round _ _ _ _ _ (OptionPool (OptionPoolPercent 0.0) _) _)) = ops c $ pr c r
ops c (Just r@(Round _ _ (PreMoneyValuation _) _ _ (OptionPool (OptionPoolPercent p) PreMoney) _)) =
  floor $ ((postmv c r) * p) / pps c r
ops c (Just r@(Round _ _ _ _ _ (OptionPool (OptionPoolPercent p) _) _)) = max ops' props
  where tsops pr' = foldr ((+) . shares (pps c r)) 0 (r ^. field @"investments") + (ts c pr') - props
        props = ops c $ pr c r
        ops' = floor $ (fromIntegral (tsops (pr c r)) / (1.0 - p)) * p

-- | The option pool percentage of a round.
opp :: Round -> Double
opp (Round _ _ _ _ _ (OptionPool (OptionPoolPercent p) _) _) = p
opp _ = error "Calling opp on a round with option pool shares specified."
