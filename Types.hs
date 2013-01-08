module Types where

import Control.Applicative;
import Control.Category.Unicode;
import Data.Array;
import Data.Binary;
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

newtype BarNumber = BarNumber Int deriving (Eq, Ord, Ix, Show, Num, Enum, Real, Integral, Binary);
newtype StaffNumber = StaffNumber Int deriving (Eq, Ord, Ix, Show, Num, Enum, Real, Integral, Binary);

type Octave = Int;

newtype PLtr = PLtr Char deriving (Eq, Ord, Binary);

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

{-!
deriving instance Binary PitchClass
deriving instance Binary Pitch
deriving instance Binary Length
deriving instance Binary Time
deriving instance Binary NR
deriving instance Binary Clef
deriving instance Binary Bar
deriving instance Binary Piece
!-}

-- GENERATED START

 
instance Binary PitchClass where
        put (PitchClass x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (PitchClass x1 x2)

 
instance Binary Pitch where
        put (Pitch x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Pitch x1 x2)

 
instance Binary Length where
        put (Length x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Length x1 x2)

 
instance Binary Time where
        put (Time x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Time x1 x2)

 
instance Binary NR where
        put (NR x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (NR x1 x2)

 
instance Binary Clef where
        put (Clef x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Clef x1 x2)

 
instance Binary Bar where
        put (Bar x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (Bar x1 x2 x3)

 
instance Binary Piece where
        put (Piece x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Piece x1 x2)
-- GENERATED STOP
