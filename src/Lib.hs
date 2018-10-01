{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import GHC.Generics
import Data.Functor.Compose
import Data.Functor.Classes
import Test.QuickCheck
import Control.Monad
import Data.Bifunctor


roughTreeSize :: (Integral a, Num a) => a
roughTreeSize = 2^(16 :: Int)


data Expr1 where
  One :: Expr1
  App :: Expr1 -> Expr1 -> Expr1
  deriving (Eq, Ord, Show)

instance Arbitrary Expr1 where
  arbitrary = frequency [(n-1, return One), (1, liftM2 App arbitrary arbitrary)]
    where
      n = roughTreeSize

expr12 :: Expr1 -> Expr2
expr12 One = Expr2 Nothing
expr12 ~(App x y) = Expr2 (Just (expr12 x, expr12 y))

expr21 :: Expr2 -> Expr1
expr21 (Expr2 Nothing) = One
expr21 (Expr2 ~(Just (x, y))) = App (expr21 x) (expr21 y)

prop_expr121 :: Expr1 -> Bool
prop_expr121 x = expr21 (expr12 x) == x

prop_expr212 :: Expr2 -> Bool
prop_expr212 x = expr12 (expr21 x) == x


newtype Expr2 = Expr2 { getExpr2 :: Maybe (Expr2, Expr2) } deriving (Eq, Ord, Show)

instance Arbitrary Expr2 where
  arbitrary = frequency [(n-1, return (Expr2 Nothing)), (1, Expr2 . Just <$> liftM2 (,) arbitrary arbitrary)]
    where
      n = roughTreeSize


expr23 :: Expr2 -> Expr3
expr23 (Expr2 Nothing) = Expr3 (Fix (Compose Nothing))
expr23 (Expr2 ~(Just (x, y))) = Expr3 (Fix (Compose (Just (Join (getExpr3 (expr23 x), getExpr3 (expr23 y))))))

expr32 :: Expr3 -> Expr2
expr32 (Expr3 (Fix (Compose Nothing))) = Expr2 Nothing
expr32 (Expr3 (Fix (Compose ~(Just (Join (x, y)))))) = Expr2 (Just (expr32 (Expr3 x), expr32 (Expr3 y)))


prop_expr232 :: Expr2 -> Bool
prop_expr232 x = expr32 (expr23 x) == x

prop_expr323 :: Expr3 -> Bool
prop_expr323 x = expr23 (expr32 x) == x


newtype Fix f = Fix { runFix :: f (Fix f) }

instance Arbitrary1 f => Arbitrary (Fix f) where
  arbitrary = Fix <$> arbitrary1

instance Eq1 f => Eq (Fix f) where
  (Fix x) == (Fix y) = eq1 x y

instance Show1 f => Show (Fix f) where
  showsPrec n (Fix x) = showsUnaryWith showsPrec1 "Fix" n x



newtype Join f a = Join { getJoin :: f a a } deriving (Generic)

instance Arbitrary2 f => Arbitrary1 (Join f) where
  liftArbitrary f = Join <$> liftArbitrary2 f f

instance Bifunctor f => Functor (Join f) where
  fmap f (Join x) = Join (bimap f f x)

instance Eq2 f => Eq1 (Join f) where
  liftEq f (Join x) (Join y) = liftEq2 f f x y

instance (Eq2 f, Eq a) => Eq (Join f a) where
  (==) = eq1

instance Show2 f => Show1 (Join f) where
  liftShowsPrec sPrec sList n (Join x) = showsUnaryWith (liftShowsPrec2 sPrec sList sPrec sList) "Join" n x

instance (Show2 f, Show a) => Show (Join f a) where
  showsPrec = showsPrec1


newtype Expr3 = Expr3 { getExpr3 :: Fix (Compose Maybe (Join (,))) } deriving (Eq, Show, Generic)

instance Arbitrary Expr3 where
  arbitrary = trimExpr3 n . Expr3 <$> arbitrary
    where
      n = roughTreeSize

trimExpr3 :: Int -> Expr3 -> Expr3
trimExpr3 0 _                       = Expr3 (Fix (Compose Nothing))
trimExpr3 _ (Expr3 (Fix (Compose Nothing))) = Expr3 (Fix (Compose Nothing))
trimExpr3 n (Expr3 (Fix (Compose ~(Just (Join (x, y)))))) = Expr3 (Fix (Compose (Just (Join (getExpr3 $ trimExpr3 (div n 2) (Expr3 x), getExpr3 $ trimExpr3 (div n 2) (Expr3 y))))))


prop_expr32123 :: Expr3 -> Bool
prop_expr32123 x = expr23 (expr12 (expr21 (expr32 x))) == x

prop_expr23132 :: Expr2 -> Bool
prop_expr23132 x = expr32 (expr23 (expr12 (expr21 x))) == x


return []
-- | Some runs:
--
-- @
-- (2.67 secs, 1,402,814,856 bytes)
-- (2.80 secs, 1,444,506,288 bytes)
-- (2.92 secs, 1,520,679,304 bytes)
-- (2.66 secs, 1,419,868,048 bytes)
-- (3.18 secs, 1,524,964,144 bytes)
-- @
--
runTests :: IO ()
runTests = $quickCheckAll >>= print


