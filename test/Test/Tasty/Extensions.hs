{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Tasty.Extensions where

import           Data.Foldable
import           Data.Proxy
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit          as H
import           Test.Tasty.QuickCheck     as QC

functorLaws :: forall m a b c. (
  Arbitrary (m (a, b, c)), Show (m (a, b, c)),
  Functor m, Arbitrary a, Arbitrary b, Arbitrary c,
  CoArbitrary a, CoArbitrary b,
  Show (m a), Arbitrary (m a),
  EqProp (m a), EqProp (m c)) => Proxy (m (a, b, c)) -> TestTree

functorLaws _ = prop "check functor laws" $ \(p :: m (a, b, c)) ->
  snd $ fold $ unbatch $ functor p :: Property

applicativeLaws :: forall m a b c. (
  Arbitrary (m (a, b, c)), Show (m (a, b, c)),
  Applicative m, Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary (m a),
  Arbitrary (m (b -> c)), Show (m (b -> c)),
  Arbitrary (m (a -> b)), Show (m (a -> b)),
  Show a, Show (m a),
  EqProp (m a), EqProp (m b), EqProp (m c)) => Proxy (m (a, b, c)) -> TestTree

applicativeLaws _ = prop "check applicative laws" $ \(p :: m (a, b, c)) ->
  snd $ fold $ unbatch $ applicative p :: Property

monadLaws :: forall m a b c. (
   Arbitrary (m (a, b, c)), Show (m (a, b, c)),
   Monad m, Show a,
   Arbitrary a, CoArbitrary a,
   Arbitrary b, CoArbitrary b,
   Arbitrary (m a), EqProp (m a), Show (m a),
   Arbitrary (m b), EqProp (m b),
   Arbitrary (m c), EqProp (m c)) => Proxy (m (a, b, c)) -> TestTree

monadLaws _ = prop "check monad laws" $ \(p :: m (a, b, c)) ->
  snd $ fold $ unbatch $ monad p :: Property

minTestsOk :: Int -> (TestTree -> TestTree)
minTestsOk n = localOption (QuickCheckTests n)

instance Monoid Property where
  mempty = label "ok" True
  mappend = (.&.)

prop :: Testable a => String -> a -> TestTree
prop = QC.testProperty

eg :: String -> Bool -> TestTree
eg name b = H.testCase name (assertBool name b)
