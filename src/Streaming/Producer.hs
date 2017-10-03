{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

Module      : Streaming.Producer
Description : Simple data type for streaming and associated combinators
Copyright   : (c) Eric Torreborre, 2017
                  Someone Else, 2014
License     : MIT
Maintainer  : etorreborre@yahoo.com
Stability   : experimental

-}
module Streaming.Producer (
    Producer(..)
  , Stream(..)
  , done
  , one
  , more
  , emit
  , append
  , filter
  , take
  , drop
  , chunk
  , runList
  , runChunks
) where

import           Control.Applicative   (liftA2)
import           Data.Functor.Identity
import qualified Data.List             as DL (drop, filter, take)
import           Prelude               hiding (drop, filter, take)

-- data types

-- | A Producer generates values of type a with effects of type m
newtype Producer m a =
  Producer { runStream :: m (Stream m a) }


-- | ADT for the produced elements. There are either:
--
--      * no element
--
--      * one element
--
--      * several elements (a "chunk") followed by the next producer
--
data Stream m a =
    Done
  | One a
  | More [a] (Producer m a)

-- |
-- == Constructors

-- | A Producer with no elements
done :: Applicative m => Producer m a
done = Producer (pure Done)

-- | A Producer with one element
one :: Applicative m => a -> Producer m a
one a = Producer (pure (One a))

-- | A Producer with n elements
emit :: Applicative m => [a] -> Producer m a
emit as = more as done

-- | A Producer with n elements and another Producer
more :: Applicative m => [a] -> Producer m a -> Producer m a
more as next = Producer (pure (More as next))

-- |
-- == Combinators

-- | Append 2 Producers together
append :: Applicative m => Producer m a -> Producer m a -> Producer m a
append (Producer s1) (Producer s2) =  Producer (liftA2 appendStream s1 s2)

appendStream :: Applicative m => Stream m a -> Stream m a -> Stream m a
appendStream s Done = s
appendStream Done s = s
appendStream (One a) (More as next) = More (a:as) next
appendStream (One a1) (One a2) = More [a1, a2] done
appendStream (More as1 (Producer next)) s2 = More as1 (Producer (fmap (`appendStream` s2) next))

mapStream :: Monad m => (Stream m a -> Producer m a) -> Producer m a -> Producer m a
mapStream f p = Producer $ runStream p >>= \s -> runStream (f s)

-- | Filter the values of a Producer
filter :: Monad m => (a -> Bool) -> Producer m a -> Producer m a
filter = mapStream . filterStream

filterStream :: Monad m => (a -> Bool) -> Stream m a -> Producer m a
filterStream _ Done = done
filterStream f (One a) = if f a then one a else done
filterStream f (More as next) =
  more (DL.filter f as) (filter f next)

-- | Take the first n elements
-- If n <= 0 an empty Producer is returned
take :: Monad m => Int -> Producer m a -> Producer m a
take = mapStream . takeStream

takeStream :: Monad m => Int -> Stream m a -> Producer m a
takeStream _ Done = done
takeStream n (One a) = if n <= 0 then done else one a
takeStream n (More as next) =
  let diff = n - length as in
    if diff > 0 then
      more as (take diff next)
    else
      more (DL.take n as) done

-- | Drop the first n elements
-- If n <= 0 nothing is dropped
drop :: Monad m => Int -> Producer m a -> Producer m a
drop = mapStream . dropStream

dropStream :: Monad m => Int -> Stream m a -> Producer m a
dropStream _ Done = done
dropStream n (One a) = if n > 0 then done else one a
dropStream n (More as next) =
  let diff = n - length as in
    if diff >= 0 then
      drop diff next
    else
      more (DL.drop n as) next

-- | Make sure that the underlying chunks have a size n
-- as much as possible
chunk :: Monad m => Int -> Producer m a -> Producer m a
chunk = mapStream . chunkStream

chunkStream :: forall m a . Monad m => Int -> Stream m a -> Producer m a
chunkStream _ Done    = done
chunkStream _ (One a) = one a
chunkStream n (More as next) =
  if n <= 0 then more as next
  else
    go [] (More as next)
    where
      go :: [a] -> Stream m a -> Producer m a
      go acc Done = emit acc

      go acc (One a) =
        emit (acc ++ [a])

      go acc (More as1 next1) =
        let needed = n - length acc in
          if length as1 >= needed then
            emit (acc ++ DL.take needed as1) `append`
            chunk n (more (DL.drop needed as1) next1)
          else
            mapStream (go (acc ++ as1)) next1

-- |
-- == Observations
-- The following functions can "run" a Producer to get values back

-- | return a list of values
runList :: Monad m => Producer m a -> m [a]
runList (Producer ma) = ma >>= runListStream

runListStream :: Monad m => Stream m a -> m [a]
runListStream Done           = pure []
runListStream (One a)        = pure [a]
runListStream (More as next) =  (\x -> as ++ x) <$> runList next

-- | return a list of chunks
runChunks :: Monad m => Producer m a -> m [[a]]
runChunks (Producer ma) = ma >>= runChunksStream

runChunksStream :: forall m a . Monad m => Stream m a -> m [[a]]
runChunksStream Done           = pure []
runChunksStream (One a)        = pure [[a]]
runChunksStream (More as next) =
  appendChunk <$> runChunks next
  where
    appendChunk :: [[a]] -> [[a]]
    appendChunk []     = [as]
    appendChunk [[]]   = [as]
    appendChunk (c:cs) = as:c:cs

-- Instances
instance (Show a) => Show (Producer Identity a) where
  show p = show (runList p)

instance (Functor m) => Functor (Producer m) where
  fmap f (Producer s) = Producer (fmap (fmap f) s)

instance (Monad m) => Applicative (Producer m) where
    pure a = Producer (return (One a))

    (<*>) p1 p2 = p1 >>= (`fmap` p2)

instance (Monad m) => Monad (Producer m) where
  return a = Producer (return (One a))

  Producer s >>= f = Producer (s >>= f') where
    f' Done               = pure Done
    f' (One a)            = runStream (f a)
    f' (More [] next)     = runStream (next >>= f)
    f' (More (a:as) next) = runStream (f a `append` (more as next >>= f))

instance (Functor m) => Functor (Stream m) where
  fmap _ Done           = Done
  fmap f (One a)        = One (f a)
  fmap f (More as next) = More (fmap f as) (fmap f next)
