{-# LANGUAGE DeriveFunctor, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Util where

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad;
import Data.Monoid;
import Data.Traversable;

infix 1 ?;
(?) :: Bool -> a -> a -> a;
True  ? x = const x;
False ? _ = id;

infixr 9 &;
(&) :: (a -> b) -> (b -> c) -> (a -> c);
(f & g) x = g (f x);

list :: (a -> [a] -> b) -> b -> [a] -> b;
list _ x    []  = x;
list f _ (y:ys) = f y ys;

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d;
uncurry3 f (x, y, z) = f x y z;

fst3 :: (a, b, c) -> a;
fst3 (x, _, _) = x;

snd3 :: (a, b, c) -> b;
snd3 (_, y, _) = y;

þrd3 :: (a, b, c) -> c;
þrd3 (_, _, z) = z;

swap :: (a, b) -> (b, a);
swap (x, y) = (y, x);

tripleA :: Arrow a => a b1 c1 -> a b2 c2 -> a b3 c3 -> a (b1, b2, b3) (c1, c2, c3);
tripleA f1 f2 f3 = (\ (x, y, z) -> (x, (y, z))) ^>> f1 *** f2 *** f3 >>^ (\ (x, (y, z)) -> (x, y, z));

maxBy, minBy :: (a -> a -> Ordering) -> a -> a -> a;
maxBy f x y | f x y == GT = x
            | otherwise   = y
            ;
minBy = maxBy ∘ flip;

deriving instance Functor First;
deriving instance Functor Last;
deriving instance Applicative First;
deriving instance Applicative Last;
