module Util.Array where

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad;
import Data.Array;
import Data.Traversable;
import Util;

transpose :: (Ix i, Ix j) => Array (i, j) a -> Array (j, i) a;
transpose a = ixmap (swap *** swap <<< bounds $ a) swap a;

cols :: (Ix i, Ix j) => Array (i, j) a -> [Array i a];
cols = transpose & rows;

rows :: (Ix i, Ix j) => Array (i, j) a -> [Array j a];
rows = join $ \ a -> traverse (ixmap (snd *** snd <<< bounds $ a) ∘ (,)) (range <<< fst *** fst <<< bounds $ a);

colMap :: (Ix i, Ix j) => (j -> Array i a -> Array i b) -> Array (i, j) a -> Array (i, j) b;
colMap f = join $ \ a -> array (bounds a) ∘ concat ∘ traverse (\ j -> fmap (flip (,) j *** id) ∘ assocs ∘ f j ∘ ixmap (fst *** fst <<< bounds $ a) (flip (,) j)) (range <<< snd *** snd <<< bounds $ a);

rowMap :: (Ix i, Ix j) => (i -> Array j a -> Array j b) -> Array (i, j) a -> Array (i, j) b;
rowMap f = transpose & colMap f & transpose;

-- Nothing means append
insertCol :: (Num j, Ix i, Ix j) => Maybe j -> Array i a -> Array (i, j) a -> Array (i, j) a;
insertCol m_j b = transpose & insertRow m_j b & transpose;

insertRow :: (Num i, Ix i, Ix j) => Maybe i -> Array j a -> Array (i, j) a -> Array (i, j) a;
insertRow (Just i) b = join $ \ a -> flip (//) (((,) i *** id) <$> assocs b) ∘ ixmap (id *** (+ 1) *** id <<< bounds $ a) (join (\ i' -> i' > i ? (+ negate 1) $ id) *** id);
insertRow Nothing  b = join $ \ a -> insertRow (Just ∘ fst ∘ snd ∘ bounds $ a) b;

insertCol1 :: (Num j, Ix i, Ix j) => Maybe j -> a -> Array (i, j) a -> Array (i, j) a;
insertCol1 m_j x = join $ \ a -> insertCol m_j (listArray (fst *** fst <<< bounds $ a) (repeat x));

insertRow1 :: (Num i, Ix i, Ix j) => Maybe i -> a -> Array (i, j) a -> Array (i, j) a;
insertRow1 m_i x = transpose & insertCol1 m_i x & transpose;

deleteCol :: (Num j, Ix i, Ix j) => j -> Array (i, j) a -> Array (i, j) a;
deleteCol j = transpose & deleteRow j & transpose;

deleteRow :: (Num i, Ix i, Ix j) => i -> Array (i, j) a -> Array (i, j) a;
deleteRow i = join $ \ a -> ixmap (id *** (+ negate 1) *** id <<< bounds $ a) (join (\ i' -> i' >= i ? (+ 1) $ id) *** id);

mapWithIx :: (Ix i) => (i -> a -> b) -> Array i a -> Array i b;
mapWithIx f a = listArray (bounds a) $ uncurry f <$> assocs a;

modifyAt :: (Ix i) => i -> (a -> a) -> Array i a -> Array i a;
modifyAt i f a = a // [(i, f (a ! i))];

insertElem :: (Num i, Ix i) => Maybe i -> a -> Array i a -> Array i a;
insertElem m_i x = join $ \ a -> ixmap ((,) () *** (,) () <<< bounds $ a) snd & insertCol1 m_i x & ixmap (id *** (+ 1) <<< bounds $ a) ((,) ());

(?!) :: (Ix i) => Array i a -> i -> Maybe a;
a ?! i | bounds a `inRange` i = Just (a ! i)
       | otherwise = Nothing
       ;
