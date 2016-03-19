module Saraswati.Music.Rep where

import Saraswati.Music.WesternNotation

class PitchedMusic a where
  getPitch :: a -> Integer

instance PitchedMusic WesternNotation where
  getPitch = Saraswati.Music.WesternNotation.getPitch

{-instance PitchedMusic OtherNotation where
  getPitch = Saraswati.Music.OtherNotation.getPitch-}
