{-# LANGUAGE TemplateHaskell #-}

module CapTable.Types
    ( module CapTable.Types
    ) where

import           Control.Lens
import           Data.Time

data Company = Company {
  _rounds   :: [Round],
  _founders :: [Investment]
} deriving Show

data Valuation = PreMoneyValuation Double | PricePerShare Double | PostEquityStake Double deriving Show

data Dividend = Dividend {
  _dividendYield     :: Double,
  _dividendCompounds :: Bool
} deriving Show

data Liquidation = Liquidation {
  _liquidationPref :: Double,
  _liquidationCap  :: Double
} deriving Show

data Preferred = Preferred {
  _dividend    :: Dividend,
  _liquidation :: Liquidation
} deriving Show

data StockType = Common | ConvertiblePreferred Preferred | ParticipatingPreferred Preferred deriving Show

data OptionPoolCreation = PreMoney | PostMoney deriving Show

data OptionPoolSize = OptionPoolPercent Double | OptionPoolShares Integer deriving Show

data OptionPool = OptionPool {
  _optionPoolSize     :: OptionPoolSize,
  _optionPoolCreation :: OptionPoolCreation
} deriving Show

data AntiDilution = None | FullRatchet | WeightedAverage | NoPriceBased deriving Show

data Round = Round {
  _roundName    :: String,
  _roundDate    :: Day,
  _valuation    :: Valuation,
  _investments  :: [Investment],
  _stockType    :: StockType,
  _optionPool   :: OptionPool,
  _antiDilution :: AntiDilution
} deriving Show

instance Ord Round where
  compare r1 r2 = _roundDate r1 `compare` _roundDate r2

instance Eq Round where
  r1 == r2 = _roundDate r1 == _roundDate r2

data InvestmentBasis = Currency Double | Shares Integer deriving Show

data Investment = Investment {
  _investor        :: Investor,
  _investmentBasis :: InvestmentBasis
} deriving Show

data Investor = Investor {
  _investorName :: String
} deriving Show

data Exit = Exit {
  _exitDate   :: Day,
  _exitAmount :: Double
} deriving Show

data Calculation = Calculation {
  _company :: Company,
  _exit    :: Maybe Exit
} deriving Show

makeLenses ''Company
makeLenses ''Dividend
makeLenses ''Liquidation
makeLenses ''Preferred
makeLenses ''OptionPool
makeLenses ''AntiDilution
makeLenses ''Round
makeLenses ''Investment
makeLenses ''Investor
makeLenses ''Exit
makeLenses ''Calculation
