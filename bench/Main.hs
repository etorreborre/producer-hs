{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import           Criterion.Main
import           Streaming.Producer

main = defaultMain [
  bgroup "producer" [
      bench "bind 1000 times"    $ nfIO (bind 1000)
    , bench "append 1000 times"  $ nfIO (appendN 1000)
    ]
  ]

bind :: Int -> IO ()
bind n = runUnit $ bindProducer n

bindProducer :: Int -> Producer IO Int
bindProducer n
  | n <= 0 = done
  | otherwise = emit [n] >>= \i -> bindProducer (i - 1)

appendN :: Int -> IO ()
appendN n = runUnit $ appendProducer n

appendProducer :: Int -> Producer IO Int
appendProducer n
  | n <= 0 = done
  | otherwise = one n `append` appendProducer (n - 1)
