import           CapTable
import           CapTable.Types
import           Control.Monad.State
import           Data.Time.Calendar
import           Test.Hspec

main :: IO ()
main = hspec $ do
    let angel = Investor "Angel"
    let i = Investment angel (Currency 2000000)
    describe "CapTable" $ do
      it "calculates a simple pps" $ do
        let i = Investment angel (Currency 2000000)
        let r = Round "Seed" (fromGregorian 2000 1 1) (PreMoneyValuation 3000000) [i] Common (OptionPool (OptionPoolPercent 0.0) PostMoney) None False
        let c = Company [r] [Investment (Investor "Founder 1") (Shares 750000), Investment (Investor "Founder 2") (Shares 750000)]
        let pps' = pps (Calculation c Nothing) r
        let ts' = ts (Calculation c Nothing) (Just r) 
        pps' `shouldBe` 2.00
        ts' `shouldBe` 2500000
      it "calculates pps given a post money option pool" $ do
        let i = Investment angel (Currency 1000000)
        let r = Round "Seed" (fromGregorian 2000 1 1) (PreMoneyValuation 4000000) [i] Common (OptionPool (OptionPoolPercent 0.15) PostMoney) None False
        let c = Company [r] [Investment (Investor "Founder 1") (Shares 2000000), Investment (Investor "Founder 2") (Shares 2000000)]
        let result = pps (Calculation c Nothing) r
        result `shouldBe` 1.00
      it "calculates the pre money option pool shares" $ do
        let i = Investment angel (Currency 1000000)
        let r = Round "Seed" (fromGregorian 2000 1 1) (PreMoneyValuation 1000000) [i] Common (OptionPool (OptionPoolPercent 0.2) PreMoney) None False
        let c = Company [r] [Investment (Investor "Founder 1") (Shares 500000), Investment (Investor "Founder 2") (Shares 500000)]
        let pps' = pps (Calculation c Nothing) r
        let result = ops (Calculation c Nothing) (Just r)
        pps' `shouldBe` 0.600000240000096 -- Look, I don't like this any more than you do.
        result `shouldBe` 666666
