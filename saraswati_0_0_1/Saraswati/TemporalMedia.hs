{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}


module Saraswati.TemporalMedia (
    Temporal(..)
  , Duration
  , Temporable(..)
  , toList
  , toVoices
  , horizontal
  ) where

import Vivid

data Temporal a where
  --TemporalUnit :: Temporable a => a -> Temporal a
  TemporalUnit :: a -> Temporal a
  (:+:) :: Temporal a -> Temporal a -> Temporal a
  (:=:) :: Temporal a -> Temporal a -> Temporal a
  deriving (Show)

class Temporable a where
  emptyUnit :: a
  getDur :: a -> Duration
  renderMedia :: Temporal a -> IO()
  --renderMedia probably could be much smaller
  --as in, we can put more of it in this file,
  --so that each instance of Temporable is easier to write
  --Maybe we just need renderMediaUnit

-- | sequential (horizontal) composition
infixr 5 :+:
-- | parallel (vertical) composition
infixr 5 :=:

type Duration = Rational
type Onset = Rational

fromList :: [(Onset,a)] -> Temporal a
fromList = undefined --havent got to this yet
toList :: Temporable a => Temporal a -> [(Onset,a)]
toList = perf' 0
toVoices :: Temporable a => [(Onset,a)] -> [[(Onset,a)]] -> [[(Onset,a)]]
toVoices [] vs = vs
toVoices (m:ms) voices =
  let
    getOnset = fst
    getTemporal = snd
    endTime :: Temporable a => [(Onset,a)] -> Duration
    endTime = \case
      [] -> 0
      evts -> (\(on,d) -> on + getDur d) $ last evts
    addEventToVoices [] m = [[m]]
    addEventToVoices (v:vs) m =
      if getOnset m < endTime v --if we play anote befoer current is over
        then v : addEventToVoices vs m
        else (v++[m]) : vs
  in
    toVoices ms $ addEventToVoices voices m

-- | this is where we define play order - the semantics of :+: and :=:
perf' :: Temporable a => Onset -> Temporal a -> [(Onset,a)]
perf' o m =
  case m of
     TemporalUnit x -> [(o, x)]
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

instance (Eq a, Temporable a) => Eq (Temporal a) where
  (==) a b = toList a == toList b

instance Functor Temporal where
  fmap  f (TemporalUnit a) = TemporalUnit $ f a
  fmap  f (m1 :+: m2)   = fmap f m1 :+: fmap f m2
  fmap  f (m1 :=: m2)   = fmap f m1 :=: fmap f m2

dur :: Temporable a => Temporal a -> Duration
dur (TemporalUnit x)  = getDur x
dur (m1 :+: m2)    = dur m1   +   dur m2
dur (m1 :=: m2)    = dur m1 `max` dur m2

sequential, parallel :: Temporable a => [Temporal a] -> Temporal a
sequential [] =  TemporalUnit emptyUnit
sequential xs = foldr1 (:+:) xs
parallel [] =  TemporalUnit emptyUnit
parallel xs = foldr1 (:=:) xs

-- | these two are for Koreans
horizontal, vertical :: Temporable a => [Temporal a] -> Temporal a
horizontal = sequential
vertical = parallel

repeat      :: Temporable a => Int -> Temporal a -> Temporal a
repeat 0 m  = TemporalUnit emptyUnit
repeat n m  = m :+: Saraswati.TemporalMedia.repeat (n- 1) m

cycle    :: Temporal p -> Temporal p
cycle m  = m :+: Saraswati.TemporalMedia.cycle m
