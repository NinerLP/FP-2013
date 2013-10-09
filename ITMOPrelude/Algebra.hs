{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
--import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mzero :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- Инстансы п
instance Monoid Nat where
	mzero = Zero
	mappend = (+.)

instance Monoid Int where
	mzero = Pos Zero
	mappend = (.+.)

instance Monoid Rat where
	mzero = Rat (Pos Zero) Zero
	mappend = (%+)

instance Group Int where
	ginv = intNeg

instance Group Rat where
	ginv = ratNeg 