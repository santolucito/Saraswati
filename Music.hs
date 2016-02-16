{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Music where

  import Vivid

  data Music a where
    --SoundUnit :: Vividable a => a -> Music a
    SoundUnit :: a -> Music a
    (:+:),(:=:) :: Music a -> Music a -> Music a

  class Vividable a where
    toFreq :: a -> (Duration,Float)
    silentUnit :: a
    getDur :: a -> Duration

  -- | sequential composition
  infixr 5 :+:
  -- | parallel composition
  infixr 5 :=:

  type Duration = Rational
  type Onset = Rational

  fromList :: [(Onset,a)] -> Music a
  fromList = undefined --havent got to this yet
  toList :: Vividable a => Music a -> [(Onset,a)]
  toList = perf' 0

  -- | this is where we define play order - the semantics of :+: and :=:
  perf' :: Vividable a => Onset -> Music a -> [(Onset,a)]
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

  instance (Eq a, Vividable a) => Eq (Music a) where
    (==) a b = toList a == toList b

  instance Functor Music where
    fmap  f (SoundUnit a) = SoundUnit $ f a
    fmap  f (m1 :+: m2)   = fmap f m1 :+: fmap f m2
    fmap  f (m1 :=: m2)   = fmap f m1 :=: fmap f m2

  dur :: Vividable a => Music a -> Duration
  dur (SoundUnit x)  = getDur x
  dur (m1 :+: m2)    = dur m1   +   dur m2
  dur (m1 :=: m2)    = dur m1 `max` dur m2

  line, chord :: Vividable a => [Music a] -> Music a
  line [] =  SoundUnit silentUnit
  line xs = foldr1 (:+:) xs
  chord [] =  SoundUnit silentUnit
  chord xs = foldr1 (:=:) xs

  repeat      :: Vividable a => Int -> Music a -> Music a
  repeat 0 m  = SoundUnit silentUnit
  repeat n m  = m :+: Music.repeat (n- 1) m

  cycle    :: Music p -> Music p
  cycle m  = m :+: Music.cycle m
