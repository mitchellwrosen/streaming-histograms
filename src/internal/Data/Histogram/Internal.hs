{-# language BangPatterns        #-}
{-# language LambdaCase          #-}
{-# language NamedFieldPuns      #-}
{-# language ScopedTypeVariables #-}

{-# options_ghc -funbox-strict-fields #-}

module Data.Histogram.Internal where

import Data.Sequence (Seq(Empty, (:<|)), (<|), (|>))

import qualified Data.Sequence as Seq

data Hist a = Hist
  { epsilon :: !Double
  , compressEvery :: !Int
  , size :: !Int
  , tuples :: !(Seq (Tuple a))
  } deriving Show

data Tuple a = Tuple
  { value :: !a
  , tupG :: !Int
  , tupD :: !Int
  } deriving Show

data Range = Range
  { rmin :: !Int
  , rmax :: !Int
  } deriving Show

--------------------------------------------------------------------------------
-- Histogram creation

-- | Create an empty streaming 'Hist'.
new :: Double -> Hist a
new e =
  Hist
    { epsilon = e
    , compressEvery = floor (1 / (2*e))
    , size = 0
    , tuples = mempty
    }

--------------------------------------------------------------------------------
-- Misc. internal queries

-- | Should this 'Hist' be compressed before inserting?
shouldCompress :: Hist a -> Bool
shouldCompress Hist{size, compressEvery} =
  size > 0 && size `mod` compressEvery == 0

-- | How many ranks this histogram's quantile queries are accurate to.
rankDelta :: Hist a -> Int
rankDelta Hist{epsilon, size} =
  floor (epsilon * fromIntegral size)

tupleRanges :: Seq (Tuple a) -> Seq Range
tupleRanges =
  Seq.drop 1 . Seq.scanl step (Range 0 0)
 where
  step :: Range -> Tuple a -> Range
  step (Range i _) x =
    Range (i + tupG x) (i + tupG x + tupD x)

--------------------------------------------------------------------------------
-- Histogram queries

-- | Find the value at the given quantile. If the 'Hist' is empty, returns
-- 'Nothing'.
quantile :: Double -> Hist a -> Maybe a
quantile q hist@Hist{size, tuples} =
  if size == 0
    then
      Nothing
    else
      Just (quantileR (ceiling (q * fromIntegral size) + rankDelta hist) tuples)

-- Invariant: sequence is non-empty.
quantileR :: Int -> Seq (Tuple a) -> a
quantileR n = \case
  x :<| xs ->
    go 1 x xs
  _ ->
    error "quantileR: empty sequence"
 where
  go :: Int -> Tuple a -> Seq (Tuple a) -> a
  go !rmin x = \case
    Empty ->
      value x
    y :<| ys ->
      if rmin + tupG y + tupD y > n
        then
          value x
        else
          go (rmin + tupG y) y ys

--------------------------------------------------------------------------------
-- Histogram modifications

-- | Insert a value into a streaming 'Hist'.
insert :: Ord a => a -> Hist a -> Hist a
insert v hist =
  do_insert v (f hist)
 where
  f :: Hist a -> Hist a
  f =
    if shouldCompress hist
      then
        compress
      else
        id

do_insert :: Ord a => a -> Hist a -> Hist a
do_insert v hist@Hist{compressEvery, epsilon, size, tuples} =
  case Seq.findIndexL (\x -> value x > v) tuples of
    Nothing ->
      hist
        { size = size + 1
        , tuples = tuples |> Tuple v 1 0
        }
    Just i ->
      let
        delta :: Int
        delta =
          if i == 0 || size <= compressEvery
            then
              0
            else
              floor (2 * epsilon * fromIntegral size) - 1
      in
        hist
          { size = size + 1
          , tuples = Seq.insertAt i (Tuple v 1 delta) tuples
          }

-- | Compress the tuples in a 'Hist'.
compress :: forall a. Hist a -> Hist a
compress hist =
  case tuples hist of
    Empty ->
      hist
    x :<| xs ->
      let
        tuples' :: Seq (Tuple a)
        tuples' =
          x <| compressTuples (2 * rankDelta hist) xs
      in
        hist
          { tuples = tuples' }

compressTuples :: Int -> Seq (Tuple a) -> Seq (Tuple a)
compressTuples !n =
  foldr step mempty
 where
  step :: Tuple a -> Seq (Tuple a) -> Seq (Tuple a)
  step x = \case
    Empty ->
      Seq.singleton x
    y :<| ys ->
      if (tupG x + tupG y + tupD y) <= n
        then
          y { tupG = tupG x + tupG y } <| ys
        else
          x <| y <| ys
