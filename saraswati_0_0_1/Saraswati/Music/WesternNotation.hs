{-# LANGUAGE LambdaCase #-}

module Saraswati.Music.WesternNotation where

import Saraswati.TemporalMedia

data WesternNotation =
     Note Pitch Duration
  |  Rest Duration
  deriving (Show)

type MidiPitch  = Integer --0-127

type Pitch = (PitchClass, Octave)
type Octave = Integer
--need a better way to do this without poluting global namespace
data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
                 |  Bf | Ass | B | Bs | Bss
     deriving (Show, Eq, Read, Enum, Bounded)

instance Ord PitchClass where
  (<=) l h = (<=) (pcToInt l) (pcToInt h)

midiPitch :: Pitch -> MidiPitch
midiPitch (pc,oct)  = 12*(oct+1) + pcToInt pc
getPitch :: WesternNotation -> MidiPitch
getPitch (Note p d) = midiPitch p
getPitch (Rest d) = 0

pcToInt  :: PitchClass -> Integer
pcToInt   = \case
  Cf  -> -1;   C  -> 0;   Cs  -> 1;
  Df  -> 1;   D  -> 2;   Ds  -> 3;
  Ef  -> 3;   E  -> 4;   Es  -> 5;
  Ff  -> 4;   F  -> 5;   Fs  -> 6;
  Gf  -> 6;   G  -> 7;   Gs  -> 8;
  Af  -> 8;   A  -> 9;   As  -> 10;
  Bf  -> 10;  B  -> 11;  Bs  -> 12;

pitch     :: MidiPitch  -> Pitch
pitch ap  =
    let (oct, n) = divMod ap 12
    in  ([C,Cs,D,Ds,E,F,Fs,G,Gs,As,B] !! fromInteger n, oct-1)

-- | any functions avaible to the end user should be over Temporal
transpose     :: Integer -> Temporal WesternNotation -> Temporal WesternNotation
transpose i  =
  let
    transpose' i p = pitch (midiPitch p + i)
  in
    fmap $ changePitch (transpose' i)

changePitch  :: (Pitch -> Pitch) -> (WesternNotation -> WesternNotation)
changePitch  f (Note p d)    = Note (f p) d
changePitch  f (Rest d)      = Rest d

changeDur  :: (Duration -> Duration) -> (WesternNotation -> WesternNotation)
changeDur  f (Note p d)    = Note p (f d)
changeDur  f (Rest d)      = Rest (f d)
