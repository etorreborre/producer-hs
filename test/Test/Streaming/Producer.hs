{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Streaming.Producer where

import           Data.Functor.Identity
import qualified Data.List                 as DL
import           Prelude                   hiding (drop, filter, take)
import           Streaming.Producer
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.HUnit          as H
import           Test.Tasty.QuickCheck     as QC

properties =
  testGroup "producer properties" [
      prop_append
    , prop_filter
    , prop_take
  ]

prop_append = testGroup "append" [
    eg "sanity check - runList done" $
      null $ run done

  , eg "append done - done" $
      null $ run $ append done done

  , prop "append 2 producers" $ \(p1, p2) ->
      run (append p1 p2)  == (run p1 ++ run p2)
  ]

prop_filter = testGroup "filter" [
  prop "filter ints" $ \p ->
    let f = (> 0) in
      run (filter f p) == DL.filter f (run p)
  ]

prop_take = testGroup "take, drop, chunk" [
    prop "take values" $ \(p, n) ->
      run (take n p) == DL.take n (run p)

  , prop "drop values" $ \(p, n) ->
      run (drop n p) == DL.drop n (run p)

  , prop "chunk values returns all values" $ \(p, n) ->
      run (chunk n p) == run p

  , localOption (QuickCheckTests 1000) $ prop "chunk values returns chunks of size == n, excepted possibly for the last one" $ \(p, n) ->
     n <= 1     ||
     size p < n ||
     if n `rem` size p == 0 then
       all (\c -> n == length c) (runC (chunk n p))
     else
       all (\c -> n == length c) (dropLast (runC (chunk n p)))
  ]

-- HELPERS

instance Arbitrary ProducerInt where
  arbitrary = oneof [
      elements [done, one 1],
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

prop :: Testable a => String -> a -> TestTree
prop = QC.testProperty

eg :: String -> Bool -> TestTree
eg name b = H.testCase name (assertBool name b)

type ProducerInt = Producer Identity Int
