-- install supercollider, then the easiest way to start the server is to open the app and hit ctrl-b

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Vivid
import Music
import WesternNotation

foo = sd (74 ::I "note", 0.1 ::I "amp") $ do
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


--this is totally broken, but the last piece to making a reasonable euterpa 2.0
playMusic s m =
  forM_ (toList m) $ \note -> do
    sleep (getDur $ snd note)
    set s (fromInteger . getP $ snd note::I "note")

song = Music.cycle . Music.line $ map SoundUnit [Note (C,4) 0.1, Note (D,4) 0.4, Note (E,4) 0.1]

main = do
   s <- synth foo ()
   playMusic s song
   free s
