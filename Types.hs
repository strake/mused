module Types where

import Control.Applicative;
import Control.Category.Unicode;
import Data.Array;
import Data.Function (on);
import Data.List ((!!));
import Data.Map as Map;
import Data.Set as Set;
import Util;

data PitchClass = PitchClass PLtr Int deriving (Eq, Ord);

instance Show PitchClass where {
  show (PitchClass (PLtr x) a) = x :
    case a of {
       2 -> "x ";
       1 -> "# ";
       0 -> "  ";
      -1 -> "b ";
      -2 -> "bb";
    };
};

data Pitch = Pitch PitchClass Octave deriving (Eq, Show);

instance Ord Pitch where {
  Pitch g1 o1 `compare` Pitch g2 o2 = (o1, g1) `compare` (o2, g2);
};

-- Length n b => length = 3^n/2^(n+b)
data Length = Length Int Int deriving (Eq, Show);

nl :: Fractional a => Length -> a;
nl (Length n b) = 3^n/2^(n+b);

instance Ord Length where {
  compare = compare `on` (nl :: Length -> Rational);
};

data Time = Time Int Int;

instance Show Time where {
  show (Time n b) = show n ++ "/" ++ show (2^b);
};

data NR = NR (Maybe Pitch) Length deriving (Eq, Ord, Show);

type Key = Int;

data Clef = Clef PLtr Octave deriving (Show);

data Bar = Bar Clef Key (Map Rational (Set NR)) deriving (Show);

data Piece = Piece (Array BarNumber Time) (Array (StaffNumber, BarNumber) Bar) deriving (Show);

newtype BarNumber = BarNumber Int deriving (Eq, Ord, Ix, Show, Num, Enum, Real, Integral);
newtype StaffNumber = StaffNumber Int deriving (Eq, Ord, Ix, Show, Num, Enum, Real, Integral);

type Octave = Int;

newtype PLtr = PLtr Char deriving (Eq, Ord);

instance Show PLtr where {
  show (PLtr x) = [x];
};

instance Enum PLtr where {
  toEnum = PLtr ∘ toEnum ∘ (+ fromEnum 'A') ∘ (`mod` 8);
  fromEnum (PLtr x) = fromEnum x - fromEnum 'A';
};

keyPCs :: Key -> Set PitchClass;
keyPCs n = 
  let {
    gs = cycle $ PLtr <$> (n > 0 ? id $ reverse) ['F', 'C', 'G', 'D', 'A', 'E', 'B'];
    m = Map.fromListWith (+) $ (flip (,) (signum n) <$> take (abs n) gs) ++ (flip (,) 0 <$> take 7 gs);
  }
  in Set.fromList $ uncurry PitchClass <$> Map.assocs m;
