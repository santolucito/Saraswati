-- install supercollider, then the easiest way to start the server is to open the app and hit ctrl-b

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Vivid
import Music
import WesternNotation

mySynth = sd (74 ::I "note", 0.1 ::I "amp") $ do
   -- '~*' are math operators, and their precedence is like in
   -- math (~* binds more tightly than ~+, etc):
   s <- sinOsc (freq_ $ midiCPS (A::A "note") ~+ 100 ~* sinOsc (freq_ 30))
   -- The (A::A "note") above refers to the synthdef argument
   -- (which is initialized as '34 ::I "note"')
   s2 <- s ~* lfTri (freq_ 30)
   out 0 [s2,s2] -- each list element represents an audio out channel

-- For the type signature of any UGen ('sinOsc', 'lfTri', etc),
--   type ':i' in ghci to get its signature
-- e.g.
-- ":i lfTri" starts with
-- lfTri :: Args '["freq"] '["iphase"] a => a ->
-- The first list ('["freq"]) is all the arguments that are required. The second is ones that are optional
-- so e.g. we could either write:
-- lfTri (freq_ x)
-- or
-- lfTri (freq_ x, iphase_ y)


song = Music.cycle . Music.line $ map (SoundUnit. (uncurry Note)) [((C,4), 1), ((D,4), 2), ((E,4), 3)]

-- | use a lines::Music -> [Music] then make a synth on each Muisc
--   each synth can then play notes sequentially
--   for now, we make a new synth for every note
playLine mySynth musicalLine = do
  s <- synth mySynth ()
  forM_ musicalLine $ \note -> do
    set s (fromInteger $ getPitch note::I "note")
    sleep (getDur note)
  free s

playMusic mySynth m =
  let ml = map snd $ toList m
  in playLine mySynth ml

main = do
  playMusic mySynth song
  return ()
