{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Saraswati.Music.Render
    (playMusic
    ,defaultSynth
    ) where

import Vivid
import Saraswati.TemporalMedia
import qualified Saraswati.Music.Rep as R
import Saraswati.Music.WesternNotation

import Control.Concurrent.Async (mapConcurrently)

instance Temporable WesternNotation where
  emptyUnit = Rest 0
  getDur = \case
    Note _ d -> d
    Rest d -> d
  renderMedia = playMusic defaultSynth

playMusic ::
  (Elem "note" sdArgs, Elem "amp" sdArgs,
   Show a, Temporable a, R.PitchedMusic a) =>
  SynthDef sdArgs -> Temporal a -> IO()
playMusic mySynth m =
  let
    vs =  toVoices (toList m) [[]]
    vs' = map (map snd) vs
  in do
    mapConcurrently (playLine mySynth) vs'
    return ()

-- | use a lines::Music -> [Music] then make a synth on each Muisc
--   each synth can then play notes sequentially
--   for now, we make a new synth for every note
--playLine :: SynthDef sdArgs -> [a] -> IO()
playLine mySynth musicalLine = do
  let space = 0.05::Duration
  s <- synth mySynth ()
  forM_ musicalLine $ \note -> do
    playNote s note space
    silence s space
  free s

playNote s n space= do
  set s (fromInteger $ R.getPitch n::I "note")
  wait (getDur n - space)

silence s space = do
  set s (0::I "amp")
  wait space
  set s (1::I "amp")

-- | the defaultSynth used for generic renderMedia
--   can be replaced if using the more advanced playMusic
defaultSynth = sd (74 ::I "note", 1 ::I "amp") $ do
  s <- sinOsc (freq_ $ midiCPS (V::V "note"))
  s2 <- s ~* (V::V "amp")
  out 0 [s2,s2] -- each list element represents an audio out channel
