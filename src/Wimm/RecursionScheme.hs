{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Wimm.RecursionScheme
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- Fix point

module Wimm.RecursionScheme
    ( Fix(..),

      Algebra,
      RAlgebra,
      cata,
      bottomUp,
      para,
      zygo,
      mutu,

      Coalgebra,
      ana,
      topDown,
      apo

    ) where
      
 -- Fix point machinery
newtype Fix f = Fix {unFix :: f (Fix f)}

-- Catamorphism and friends
type Algebra f a = f a -> a
type RAlgebra f a = f (Fix f, a) -> a

-- Bottom up approach to reduce the functor into an element of a
cata :: (Functor f) =>  Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- Helper when we go bottom up from Fix f to Fix f
bottomUp :: forall f . (Functor f) => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp g = cata alg
  where alg :: Algebra f (Fix f)
        alg = g . Fix

-- Same as cata, but also provides the original sub functors
para :: forall f a . (Functor f) => RAlgebra f a -> Fix f -> a
para ralg = snd . cata alg
  where alg :: Algebra f (Fix f, a)
        alg x = (Fix $ fmap fst x, ralg x)

-- Allows for semi-mutual recursive functions, when the second functions depends
-- on the results of the first, but not the other way around. We can view the
-- first function as an helper or a function that computes a property on the
-- same functor that the second function needs.
zygo :: forall f a b . (Functor f) => Algebra f a -> (f (a, b) -> b) -> Fix f -> b
zygo g1 g2 = snd . cata alg
  where alg :: Algebra f (a,b)
        alg x = (g1 $ fmap fst x, g2 x)

-- Same as zygo, but for mutually recursive functions.
mutu :: forall f a b . (Functor f) => (f (a,b) -> a) -> (f (a, b) -> b) -> Fix f -> b
mutu g1 g2 = snd . cata alg
  where alg :: Algebra f (a, b)
        alg x = (g1 x, g2 x)

-- Anamorphisms and friends
type Coalgebra f a = a -> f a
type RCoalgebra f a = a -> f (Either (Fix f) a)

-- Producing a value of Fix f recursively from a seed a
-- Dual of catamorphism
ana :: (Functor f) => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- Helper to use anamorphism when we have a function of Fix f -> Fix f
topDown :: forall f . (Functor f) => (Fix f -> Fix f) -> Fix f -> Fix f
topDown g = ana coAlg
  where coAlg :: Coalgebra f (Fix f)
        coAlg = unFix . g

-- Dual of paramorphism
-- Producing a value of Fix f recursively from a seed or with an
-- already provided term
apo :: forall f a . (Functor f) => RCoalgebra f a -> a -> Fix f
apo rcoalg = ana coalg . Right
  where coalg :: Coalgebra f (Either (Fix f) a)
        coalg (Left t) = fmap Left $ unFix t
        coalg (Right x) = rcoalg x