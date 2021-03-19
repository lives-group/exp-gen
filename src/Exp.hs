module Exp where

import Test.QuickCheck


-- type definition

data Ty = Nat | Bool
   deriving (Eq, Ord, Show)

-- expression definition

data Exp
  = Number Int
  | Logic  Bool
  | Exp :+: Exp
  | Exp :<=: Exp
  | Exp :&: Exp
  deriving (Eq, Ord, Show)

-- type checking expressions

tc :: Exp -> Maybe Ty
tc (Number _)
  = Just Nat
tc (Logic _)
  = Just Bool
tc (e :+: e')
  = do
     t  <- tc e
     t' <- tc e'
     if (any (== Bool) [t,t']) then Nothing
       else Just Nat
tc (e :<=: e')
  = do
     t  <- tc e
     t' <- tc e'
     if (any (== Bool) [t,t']) then Nothing
       else Just Bool
tc (e :&: e')
  = do
     t  <- tc e
     t' <- tc e'
     if (any (== Nat) [t,t']) then Nothing
       else Just Bool



-- generating typed expressions

instance Arbitrary Ty where
  arbitrary = elements [Nat, Bool]

genInt :: Gen Int
genInt
  = chooseInt (0,100)

genBool :: Gen Bool
genBool = elements [False, True]

-- a type for controlling the depth
-- of expression generation.

type Depth = Int

-- generating an expression of a given input type
-- and a maximum depth.

genExp :: Ty -> Int -> Gen Exp
genExp t n
  | n <= 1
     = case t of
         Nat -> Number <$> genInt
         Bool -> Logic <$> genBool
  | otherwise
     = case t of
         Nat ->
           frequency [ (20, Number <$> genInt)
                     , (80, (:+:) <$> genExp Nat n1 <*> genExp Nat n1)]
         Bool ->
           frequency [ (20, Logic  <$> genBool)
                     , (40, (:<=:) <$> genExp Nat n1  <*> genExp Nat n1)
                     , (40, (:&:)  <$> genExp Bool n1 <*> genExp Bool n1)]
       where
         n1 = n `div` 2

instance Arbitrary Exp where
  arbitrary
    = do
        t <- arbitrary :: Gen Ty
        sized (genExp t)
