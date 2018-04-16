{-# language TemplateHaskell #-}

import Data.Histogram.Internal

import Data.List (sort)
import Data.Foldable
import Data.Sequence (Seq((:<|), (:|>)))
import Hedgehog

import qualified Data.Sequence as Seq
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = do
  let group = $$(discover)
      group' =
        group
          { groupProperties =
              map
                (\(name, prop) -> (name, withTests 1000 prop))
                (groupProperties group)
          }
  checkParallel group' >>= print

--------------------------------------------------------------------------------
-- Properties

-- The min element of a histogram isn't thrown away.
prop_min :: Property
prop_min =
  property $ do
    (hist, vals) <- forAll genHistOf1
    case tuples hist of
      x :<| _ ->
        value x === minimum vals
      _ ->
        error "prop_min: empty tuples"

-- The max element of a histogram isn't thrown away.
prop_max :: Property
prop_max =
  property $ do
    (hist, vals) <- forAll genHistOf1
    case tuples hist of
      _ :|> x ->
        value x === maximum vals
      _ ->
        error "prop_max: empty tuples"

-- Values in a histogram are sorted.
prop_sorted :: Property
prop_sorted =
  property $ do
    hist <- forAll genHist
    let vals = fmap value (tuples hist)
    vals === Seq.sort vals

-- There are no sufficiently large gaps in between a tuple's rmax and the next
-- tuple's rmin. This is another way of asking, can every quantile query be
-- satisfied?
prop_no_gaps :: Property
prop_no_gaps =
  property $ do
    hist <- forAll genHist
    let range = toList (tupleRanges (tuples hist))
    annotateShow range
    for_ (pairs range)
      (\(r1, r2) ->
        assert (rmax r1 - rmin r2 <= rankDelta hist * 2))

prop_g_sum_to_n :: Property
prop_g_sum_to_n =
  property $ do
    hist <- forAll genHist
    size hist === sum (fmap tupG (tuples hist))

-- max(g+d)/2 <= e*n
prop_corollary_1 :: Property
prop_corollary_1 =
  property $ do
    hist <- forAll genHist1
    let lhs = maximum (fmap (\x -> tupG x + tupD x) (tuples hist)) `div` 2
    let rhs = rankDelta hist
    annotate (show lhs ++ " <= " ++ show rhs)
    assert (lhs <= rhs)

-- All 'g' are >= 1.
prop_g_positive :: Property
prop_g_positive =
  property $ do
    hist <- forAll genHist
    for_ (tuples hist)
      (\x -> assert (tupG x >= 1))

-- All deltas are >= 0.
prop_delta_positive :: Property
prop_delta_positive =
  property $ do
    hist <- forAll genHist
    for_ (tuples hist)
      (\x -> assert (tupD x >= 0))

-- 'tupRange' returns a range for every tuple.
prop_range_len :: Property
prop_range_len =
  property $ do
    xs <- forAll genTuples
    length xs === length (tupleRanges xs)

-- 'quantile' returns some value from the input.
prop_quantile_elem :: Property
prop_quantile_elem =
  property $ do
    (xs, vals) <- forAll genHistOf1
    q <- forAll genQuantile
    case quantile q xs of
      Just val ->
        assert (val `elem` vals)
      _ ->
        error "prop_quantile_elem: Nothing"

-- 'quantile' returns some value whose rank is within e*N of the true value's
-- rank.
prop_quantile_close_enough :: Property
prop_quantile_close_enough =
  property $ do
    (hist, vals) <- forAll genHistOf1
    q <- forAll genQuantile
    case quantile q hist of
      Just val -> do
        annotateShow val

        let vals' = sort vals
        annotateShow vals'

        -- Get the real rank of the query, i.e. the rank we could return if we
        -- did not use a fancy pruning algorithm.
        let real_rank = ceiling (q * fromIntegral (length vals))
        annotateShow real_rank

        -- Get the rank of 'val', i.e. the rank we were given.
        let given_rank = length (takeWhile (/= val) vals') + 1
        annotateShow given_rank

        let delta = rankDelta hist
        annotateShow delta

        -- Assert that we are indeed close enough.
        assert (abs (real_rank - given_rank) <= delta)
      _ ->
        error "prop_quantile_close_enough: Nothing"

--------------------------------------------------------------------------------
-- Generators

genHist :: Gen Hist
genHist =
  fmap fst genHistOf

genHist1 :: Gen Hist
genHist1 =
  fmap fst genHistOf1

genHistOf :: Gen (Hist, [Double])
genHistOf = do
  e <- Gen.prune (Gen.double (Range.exponentialFloat 0.01 0.1))
  xs <- Gen.list (Range.linear 0 100) (Gen.prune (Gen.double (Range.constant 0 100)))
  pure (foldr insert (new e) xs, xs)

genHistOf1 :: Gen (Hist, [Double])
genHistOf1 = do
  e <- pure 0.1 -- Gen.prune (Gen.double (Range.exponentialFloat 0.01 0.1))
  xs <- Gen.list (Range.linear 1 100) (Gen.prune (Gen.double (Range.constant 0 100)))
  pure (foldr insert (new e) xs, xs)

genTuples :: Gen (Seq Tuple)
genTuples =
  fmap tuples genHist

genQuantile :: Gen Double
genQuantile =
  pure 0.1 -- Gen.prune (Gen.double (Range.constant 0 1))

--------------------------------------------------------------------------------
-- Misc.

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:y:ys) = (x,y) : pairs (y:ys)
