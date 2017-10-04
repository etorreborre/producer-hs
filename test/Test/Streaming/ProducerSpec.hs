{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Streaming.ProducerSpec where

import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.List                 as DL
import           Data.Proxy
import           Prelude                   hiding (drop, filter, take)
import           Streaming.Producer
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.Extensions
import           Test.Tasty.HUnit          as H
import           Test.Tasty.QuickCheck     as QC

test_show = prop "show" $ \(p :: ProducerInt) ->
  show p == show (runList p)

test_append = testGroup "append" [
    eg "sanity check - runList done" $
      null $ run done

  , eg "append done - done" $
      null $ run $ append done done

  , prop "append 2 producers" $ \(p1, p2) ->
      run (append p1 p2)  == (run p1 ++ run p2)
  ]

test_filter = testGroup "filter" [
  prop "filter ints with tight predicate" $ \p ->
    let f = (== 0) in
      run (filter f p) == DL.filter f (run p)

  , prop "filter ints with more accepting predicate" $ \p ->
    let f = (> 0) in
      run (filter f p) == DL.filter f (run p)
  ]

test_take = testGroup "take, drop, chunk" [
    prop "take values" $ \(p, n) ->
      run (take n p) == DL.take n (run p)

  , prop "drop values" $ \(p, n) ->
      run (drop n p) == DL.drop n (run p)

  , prop "chunk values returns all values" $ \(p, n) ->
      run (chunk n p) == run p

  , minTestsOk 1000 $ prop "chunk values returns chunks of size == n, excepted possibly for the last one" $ \(p, n) ->
     n <= 1     ||
     size p < n ||
     if n `rem` size p == 0 then
       all (\c -> n == length c) (runC (chunk n p))
     else
       all (\c -> n == length c) (dropLast (runC (chunk n p)))

  , prop "chunk values for a producer with no elements" $ \n ->
     null $ runC (chunk n done)

  , prop "chunk values for a producer with one element" $ \(n, a) ->
     runC (chunk n (one a)) == [[a]]
  ]

test_runUnit = prop "runUnit" $ \(p :: ProducerInt) ->
    runIdentity (runUnit p) == ()

test_monad = testGroup "laws" [
    functorLaws     (Proxy :: Proxy ProducerIntIntInt)
  , applicativeLaws (Proxy :: Proxy ProducerIntIntInt)
  , monadLaws       (Proxy :: Proxy ProducerIntIntInt)
  ]

-- HELPERS

instance Arbitrary ProducerInt where
  arbitrary = oneof [
      elements [done, one 1],
      more <$> arbitrary <*> arbitrary
    ]

instance Arbitrary (Producer Identity (Int -> Int)) where
  arbitrary = oneof [
      elements [done, one (+1), one (* 2)],
      more <$> arbitrary <*> arbitrary
    ]

instance Arbitrary ProducerIntIntInt where
  arbitrary =
    do a <- arbitrary
       b <- arbitrary
       c <- arbitrary
       oneof [
           elements [done, one (a :: Int, b :: Int, c :: Int)],
           more <$> arbitrary <*> arbitrary
         ]

run :: ProducerInt -> [Int]
run = runIdentity . runList

size :: ProducerInt -> Int
size = length . run

empty :: ProducerInt -> Bool
empty p = size p == 0

dropLast :: [a] -> [a]
dropLast []     = []
dropLast [_]    = []
dropLast (a:as) = a : dropLast as

runC :: ProducerInt -> [[Int]]
runC = runIdentity . runChunks

type ProducerInt = Producer Identity Int
type ProducerIntIntInt = Producer Identity (Int, Int, Int)

instance Eq ProducerInt where
  (==) p1 p2 = runList p1 == runList p2

instance EqProp ProducerInt where
  (=-=) = eq
