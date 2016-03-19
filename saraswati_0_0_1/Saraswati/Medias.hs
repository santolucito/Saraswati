{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}


module Saraswati.Medias (
    Media(..)
  , Duration
  , Mediable(..)
  , toList
  , toVoices
  , horizontal
  ) where

import Vivid

data Media a where
  --MediaUnit :: Mediable a => a -> Media a
  MediaUnit :: a -> Media a
  (:+:) :: Media a -> Media a -> Media a
  (:=:) :: Media a -> Media a -> Media a
  deriving (Show)

class Mediable a where
  emptyUnit :: a
  getDur :: a -> Duration
  renderMedia :: Media a -> IO() --this probably could be much smaller

-- | sequential (horizontal) composition
infixr 5 :+:
-- | parallel (vertical) composition
infixr 5 :=:

type Duration = Rational
type Onset = Rational

fromList :: [(Onset,a)] -> Media a
fromList = undefined --havent got to this yet
toList :: Mediable a => Media a -> [(Onset,a)]
toList = perf' 0
toVoices :: Mediable a => [(Onset,a)] -> [[(Onset,a)]] -> [[(Onset,a)]]
toVoices [] vs = vs
toVoices (m:ms) voices =
  let
    getOnset = fst
    getMedia = snd
    endTime :: Mediable a => [(Onset,a)] -> Duration
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
perf' :: Mediable a => Onset -> Media a -> [(Onset,a)]
perf' o m =
  case m of
     MediaUnit x -> [(o, x)]
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

instance (Eq a, Mediable a) => Eq (Media a) where
  (==) a b = toList a == toList b

instance Functor Media where
  fmap  f (MediaUnit a) = MediaUnit $ f a
  fmap  f (m1 :+: m2)   = fmap f m1 :+: fmap f m2
  fmap  f (m1 :=: m2)   = fmap f m1 :=: fmap f m2

dur :: Mediable a => Media a -> Duration
dur (MediaUnit x)  = getDur x
dur (m1 :+: m2)    = dur m1   +   dur m2
dur (m1 :=: m2)    = dur m1 `max` dur m2

sequential, parallel :: Mediable a => [Media a] -> Media a
sequential [] =  MediaUnit emptyUnit
sequential xs = foldr1 (:+:) xs
parallel [] =  MediaUnit emptyUnit
parallel xs = foldr1 (:=:) xs

-- | these two are for Koreans
horizontal, vertical :: Mediable a => [Media a] -> Media a
horizontal = sequential
vertical = parallel

repeat      :: Mediable a => Int -> Media a -> Media a
repeat 0 m  = MediaUnit emptyUnit
repeat n m  = m :+: Saraswati.Medias.repeat (n- 1) m

cycle    :: Media p -> Media p
cycle m  = m :+: Saraswati.Medias.cycle m
