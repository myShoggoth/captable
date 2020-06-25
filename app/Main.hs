module Main where

import CapTable
import CapTable.Types
import Data.Time.Calendar
import Control.Monad.State

main :: IO ()
main = do
  let inv = Investor "Angel"
  let i = Investment inv (Currency 2000000)
  let r = Round "Seed" (fromGregorian 2000 1 1) (PreMoneyValuation 3000000) [i] Common (OptionPool (OptionPoolPercent 0.0) PreMoney) None
  let c = Company [r] [Investment (Investor "Founder 1") (Shares 750000), Investment (Investor "Founder 2") (Shares 750000)]
  let result = pps (Calculation c Nothing) r
  print $ show result
