{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tiles where

  import Vivid

  data Tile a where
    --SoundUnit :: Tileable a => a -> Tile a
    SoundUnit :: a -> Tile a
    (:+:),(:=:) :: Tile a -> Tile a -> Tile a

  class Tileable a where
    toFreq :: a -> (Duration,Float)
    silentUnit :: a
    getDur :: a -> Duration

  -- | sequential composition
  infixr 5 :+:
  -- | parallel composition
  infixr 5 :=:

  type Duration = Rational
  type Onset = Rational

  fromList :: [(Onset,a)] -> Tile a
  fromList = undefined --havent got to this yet
  toList :: Tileable a => Tile a -> [(Onset,a)]
  toList = perf' 0

  -- | this is where we define play order - the semantics of :+: and :=:
  perf' :: Tileable a => Onset -> Tile a -> [(Onset,a)]
  perf' o m =
    case m of
       SoundUnit x -> [(o, x)]
       m1 :+: m2   ->
         perf' o m1++perf' (o + dur m1) m2
       m1 :=: m2   ->
         merge (perf' o m1) (perf' o m2)

  merge :: [(Onset,a)] -> [(Onset,a)] -> [(Onset,a)]
  merge []          es2         =  es2
  merge es1         []          =  es1
  merge a@(e1:es1)  b@(e2:es2)  =
   if fst e1 < fst e2  then  e1  : merge es1 b
                       else  e2  : merge a es2

  instance (Eq a, Tileable a) => Eq (Tile a) where
    (==) a b = toList a == toList b

  instance Functor Tile where
    fmap  f (SoundUnit a) = SoundUnit $ f a
    fmap  f (m1 :+: m2)   = fmap f m1 :+: fmap f m2
    fmap  f (m1 :=: m2)   = fmap f m1 :=: fmap f m2

  dur :: Tileable a => Tile a -> Duration
  dur (SoundUnit x)  = getDur x
  dur (m1 :+: m2)    = dur m1   +   dur m2
  dur (m1 :=: m2)    = dur m1 `max` dur m2

  line, chord :: Tileable a => [Tile a] -> Tile a
  line [] =  SoundUnit silentUnit
  line xs = foldr1 (:+:) xs
  chord [] =  SoundUnit silentUnit
  chord xs = foldr1 (:=:) xs

  repeat      :: Tileable a => Int -> Tile a -> Tile a
  repeat 0 m  = SoundUnit silentUnit
  repeat n m  = m :+: Tile.repeat (n- 1) m

  cycle    :: Tile p -> Tile p
  cycle m  = m :+: Tile.cycle m
