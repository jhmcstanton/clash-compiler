{-# LANGUAGE DataKinds,TypeApplications,TemplateHaskell #-}
{-# LANGUAGE GADTs,StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module AutoRegTest where
import Clash.Explicit.Prelude

import AutoReg
import AutoRegDeriving



data MyPair a b = MkPair { getA :: a, getB :: b } deriving (Generic,NFDataX)
deriveAutoReg ''MyPair

data Tup3 a b c = MkTup3 { fA :: a, fB :: b, fC :: c } deriving (Generic,NFDataX)
deriveAutoReg ''Tup3


newtype OtherPair a b = OtherPair (MyPair a b) deriving (Generic,NFDataX)
-- deriveAutoReg ''OtherPair

data MyPair2 a b c = MkPair2 a b b deriving (Generic,NFDataX)
-- deriveAutoReg ''MyPair2

data Concrete = BoolAndInt Bool Int deriving (Generic,NFDataX)
deriveAutoReg ''Concrete

-- deriveAutoReg ''Int
deriveAutoReg ''(,)
deriveAutoReg ''(,,)


data InfixDataCon a b = a :-.- b deriving (Generic,NFDataX)
deriveAutoReg ''InfixDataCon


data TestNoFields = NoFields
-- deriveAutoReg ''TestNoFields

-- data Foo a = forall b. MkFoo a b
-- deriving instance Generic (Foo a)
-- -- ,NFDataX)




-- topEntity clk rst =
--   -- autoReg @(Maybe (Unsigned 4,BitVector 6)) @System clk rst enableGen (Just (3,5))
--   -- autoReg @(Unsigned 3,Bool,Unsigned 5) @System clk rst enableGen (2,False,3)
--   -- autoReg @(Vec 3 (Bool,Unsigned 4)) @System clk rst enableGen (((,) False) <$> 2 :> 3:>4:> Nil)
--   -- autoReg @(Concrete) @System clk rst enableGen (BoolAndInt True 1)
--   autoReg @(MyPair (Unsigned 3) Bool) @System clk rst enableGen (MkPair 2 False)
--   -- autoRegGeneric @(MyPair (Unsigned 3) Bool) @System clk rst enableGen (MkPair 2 False)

initVal :: (MyPair (Tup3 Bool (Unsigned 3) (BitVector 4))  (Tup3 Bool (Unsigned 3) (BitVector 4)) )
initVal = MkPair a a where a = (MkTup3 False 2 3)


testAutoGeneric,testAutoTH :: Clock System -> Reset System
  -- -> Signal System (Tup3 Bool (Unsigned 3) (BitVector 4))
  -- -> Signal System _ (MyPair (Tup3 Bool (Unsigned 3) (BitVector 4))  (Tup3 Bool (Unsigned 3) (BitVector 4)) )
  -> _
testAutoGeneric clk rst = autoRegGeneric clk rst enableGen initVal
testAutoTH clk rst =  autoReg clk rst enableGen initVal
{-# ANN testAutoGeneric (defSyn "testAutoGeneric") #-}
{- # ANN testAutoTH (defSyn "testAutoTH") #-}
