{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module CapTable.Types
    ( module CapTable.Types
    ) where

import           Data.Time
import           GHC.Generics

data Company = Company {
  rounds   :: [Round],
  founders :: [Investment]
} deriving stock (Show, Generic)

data Valuation = PreMoneyValuation Double | PricePerShare Double | PostEquityStake Double
  deriving Show

data Dividend = Dividend {
  dividendYield     :: Double,
  dividendCompounds :: Bool
} deriving stock (Show, Generic)

data Liquidation = Liquidation {
  liquidationPref :: Double,
  liquidationCap  :: Double
} deriving stock (Show, Generic)

data Preferred = Preferred {
  dividend    :: Dividend,
  liquidation :: Liquidation
} deriving stock (Show, Generic)

data StockType = Common | ConvertiblePreferred Preferred | ParticipatingPreferred Preferred
  deriving Show

data OptionPoolCreation = PreMoney | PostMoney deriving Show

data OptionPoolSize = OptionPoolPercent Double | OptionPoolShares Integer
  deriving Show

data OptionPool = OptionPool {
  optionPoolSize     :: OptionPoolSize,
  optionPoolCreation :: OptionPoolCreation
} deriving stock (Show, Generic)

data AntiDilution = None | FullRatchet | WeightedAverage | NoPriceBased
  deriving Show

data Round = Round {
  roundName    :: String,
  roundDate    :: Day,
  valuation    :: Valuation,
  investments  :: [Investment],
  stockType    :: StockType,
  optionPool   :: OptionPool,
  antiDilution :: AntiDilution
} deriving stock (Show, Generic)

instance Ord Round where
  compare r1 r2 = roundDate r1 `compare` roundDate r2

instance Eq Round where
  r1 == r2 = roundDate r1 == roundDate r2

data InvestmentBasis = Currency Double | Shares Integer
  deriving Show

data Investment = Investment {
  investor        :: Investor,
  investmentBasis :: InvestmentBasis
} deriving stock (Show, Generic)

data Investor = Investor {
  investorName :: String
} deriving Show

data Exit = Exit {
  exitDate   :: Day,
  exitAmount :: Double
} deriving stock (Show, Generic)

data Calculation = Calculation {
  company :: Company,
  exit    :: Maybe Exit
} deriving stock (Show, Generic)
