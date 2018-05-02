{-# OPTIONS -Wall #-}

module CapTable
(
  module CapTable
) where

import           CapTable.Types

import           Control.Lens
import           Control.Monad.State
import           Data.List

-- import           Debug.Trace

-- | Price per share for a round.
pps :: Round -> State Calculation Double
-- pps r | trace ("PPS for " ++ show r) False = undefined
pps (Round _ _ (PricePerShare p) _ _ _ _)                             = return p
pps r@(Round _ _ _ _ _ (OptionPool (OptionPoolPercent _) PreMoney) _) = do
  pmv' <- pmv r
  tspmos' <- tspmos r
  let pps' = pmv' / fromInteger tspmos'
  return pps'
pps r                                                                 = do
  pmv' <- pmv r
  pr' <- pr r
  ts' <- ts pr'
  let pps' = pmv' / fromInteger ts'
  return pps'

-- | The total pre-money option pool shares.
tspmos :: Round -> State Calculation Integer
-- tspmos r | trace ("TS PMOS for " ++ show r) False = undefined
tspmos r@(Round _ _ _ _ _ (OptionPool _ PreMoney) _) = do
  pr' <- pr r
  pmv' <- pmv r
  opp' <- opp r
  ts' <- ts pr'
  ti' <- ti r
  let pmp = ti' / (pmv' + ti')
  let pmo = 1.0 - pmp - opp'
  let prs = fromInteger ts' / pmo
  return $ floor $ prs * (pmo + opp')
tspmos _ = error "Getting pre-money option pool shares for non-premoney option pool round."

-- | Pre-Money Valuation for a round.
pmv :: Round -> State Calculation Double
-- pmv r | trace ("PMV for " ++ show r) False = undefined
pmv (Round _ _ (PreMoneyValuation p) _ _ _ _) = return p
pmv r@(Round _ _ (PricePerShare p) _ _ _ _)   = do
  pr' <- pr r
  ts' <- ts pr'
  ops' <- ops (Just r)
  return $ p * fromInteger (ts' + ops')
pmv r@(Round _ _ (PostEquityStake p) _ _ _ _) = do
  ti' <- ti r
  pr' <- pr r
  ts' <- ts pr'
  return $ (ti' / ( (1.0 - p) - fromInteger ts')) * fromInteger ts'

-- | Post-Money Valuation for a round.
postmv :: Round -> State Calculation Double
postmv r@(Round _ _ (PreMoneyValuation p) _ _ _ _)  = do
  ti' <- ti r
  return $ ti' + p
postmv r@(Round _ _ (PricePerShare _) _ _ _ _)      = do
  ti' <- ti r
  pmv' <- pmv r
  return $ ti' + pmv'
postmv (Round _ _ (PostEquityStake _) _ _ _ _)      = error "Unable to calculate post-money value due to post-equity stake."


-- | Total Shares for a round.
--   In this case Nothing refers to the foundation of the company.
ts :: Maybe Round -> State Calculation Integer
-- ts Nothing | trace "TS for Nothing" False = undefined
ts Nothing  = do
  c <- get
  return $ foldr ((+) . shares 0) 0 (c ^. company . founders)
-- ts (Just r) | trace ("TS for " ++ show r) False = undefined
ts mr@(Just r) = do
  pr' <- pr r
  ts' <- ts pr'
  props' <- ops pr'
  ops' <- ops mr
  pps' <- pps r
  return $ foldr ((+) . shares pps') 0 (r ^. investments) + ts' - props' + ops'

-- | Total monetary amount invested in a round.
ti :: Round -> State Calculation Double
-- ti r | trace ("TI for " ++ show r) False = undefined
ti r@(Round _ _ (PricePerShare p) _ _ _ _)   = return $ foldr ((+) . invested (Just p)) 0 (r ^. investments)
ti r = return $ foldr ((+) . invested Nothing) 0 (r ^. investments)

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
pr :: Round -> State Calculation (Maybe Round)
-- pr r | trace ("PR for " ++ show r) False = undefined
pr r = do
  c <- get
  let rs = sort (c ^. company . rounds)
  let mi = r `elemIndex` rs
  return $ maybeIndex mi rs
  where -- maybeIndex mIndex rnds | trace ("Getting index " ++ show mIndex ++ " of " ++ show rnds) False = undefined
        maybeIndex Nothing _   = Nothing
        maybeIndex (Just 0) _  = Nothing
        maybeIndex (Just i) rs = Just $ rs !! (i - 1)

-- | The number of option pool shares for a round.
ops :: Maybe Round -> State Calculation Integer
ops Nothing                                                           = return 0
ops (Just r@(Round _ _ _ _ _ (OptionPool (OptionPoolShares 0) _) _))  = do
  pr' <- pr r
  ops pr'
ops (Just (Round _ _ _ _ _ (OptionPool (OptionPoolShares s) _) _))    = return s
ops (Just r@(Round _ _ _ _ _ (OptionPool (OptionPoolPercent 0.0) _) _)) = do
  pr' <- pr r
  ops pr'
ops (Just r@(Round _ _ (PreMoneyValuation _) _ _ (OptionPool (OptionPoolPercent p) PreMoney) _)) = do
  postmv' <- postmv r
  let opa = postmv' * p
  pps' <- pps r
  return $ floor $ opa / pps'
ops (Just r@(Round _ _ _ _ _ (OptionPool (OptionPoolPercent p) _) _)) = do
  pr' <- pr r
  props <- ops pr'
  ts' <- tsops pr' props
  let ops' = floor $ (fromIntegral ts' / (1.0 - p)) * p
  return $ max ops' props
  where tsops pr' props = do
          tspr' <- ts pr'
          pps' <- pps r
          return $ foldr ((+) . shares pps') 0 (r ^. investments) + tspr' - props

-- | The option pool percentage of a round.
opp :: Round -> State Calculation Double
opp (Round _ _ _ _ _ (OptionPool (OptionPoolPercent p) _) _) = return p
opp _ = error "Calling opp on a round with option pool shares specified."
