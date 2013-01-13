module Draw (Style (..), note, rest, sharp, sharp2, flat, flat2, natural, space, staff, sxbars, bar, finesse) where

import Prelude hiding (foldr, maximum);
import Control.Applicative;
import Control.Category.Unicode;
import Control.Monad;
import Data.Colour;
import Data.Foldable;
import Data.Foldable.Unicode;
import Data.Function (on);
import Data.List hiding (foldr, maximum, maximumBy);
import qualified Data.Map as Map;
import qualified Data.Set as Set;
import Data.Set (Set);
import Data.Maybe;
import Data.Monoid;
import Data.Ord.Unicode;
import Diagrams.Attributes;
import Diagrams.Combinators;
import Diagrams.Core hiding (Style);
import Diagrams.Core.Envelope;
import Diagrams.Core.Points;
import Diagrams.Path;
import Diagrams.Segment;
import Diagrams.TwoD.Align;
import Diagrams.TwoD.Arc;
import Diagrams.TwoD.Combinators;
import Diagrams.TwoD.Ellipse;
import Diagrams.TwoD.Size;
import Diagrams.TwoD.Shapes;
import Diagrams.TwoD.Transform;
import Diagrams.TwoD.Types;
import Diagrams.TwoD.Vector;
import Types;
import Util;

data Style = Style {
  eccentricity :: Double
};

-- Note of minor axis 1 and given eccentricity, centred at origin, stem up, with given minus logarithm of base length
note, rest :: (PathLike (QDiagram b R2 m), Backend b R2, Monoid' m) => Style -> Int -> QDiagram b R2 m;
note s n =
  let {
    e = eccentricity s;
    b = 1/2;
    a = b/(1-e^2)**(1/2);
    l = 4;
    stem = translate (r2 (a, l/2)) (vrule l <> translateX (1/2) (strutX 1) <> foldMap (\ n -> translate (r2 (1/2, l/2 - fromIntegral n)) (hrule 1)) [0..n-3]);
    head = (n ≥ 2 ? fc black $ id) (ellipseXY a b);
  }
  in (n > 0 ? stem $ mempty) <> head;

rest s n =
  let {
    e = eccentricity s;
    b = 1/2;
    a = b/(1-e^2)**(1/2);
  }
  in case n of {
    0 -> translateY 1 ∘ alignT ∘ fc black $ rect (2*a) (1/2);
    1 ->                alignB ∘ fc black $ rect (2*a) (1/2);
    2 -> scale (3/4) ∘ translateY (-1) $ fromVertices (p2 <$> [(-1, 3), (0, 2), (-1, 1), (0, 1)]) <> arc (1/4 :: CircleFrac) (3/4 :: CircleFrac);
    n ->
      let {
        v :: R2;
        v = (1 + (fromIntegral n-3)/6)*(r2 (1, 2));
        tail = pathLike (P $ v + r2 (0, -1)) False $ (trailSegments ∘ reverseTrail) (arcT (5/8 :: CircleFrac) (7/8 :: CircleFrac));
      } in pathLike (p2 (0, -1)) False [straight v] <> cat' (negate v) (CatOpts { catMethod = Distrib, sep = 5**(1/2)/6 }) (take (n-2) $ repeat tail);
  };

sharp, sharp2, flat, flat2, natural, space :: (PathLike (QDiagram b R2 m), Backend b R2, Monoid' m) => QDiagram b R2 m;
sharp   = shearY (1/5) (translateY (1/3) (hrule 2) <> translateY (-1/3) (hrule 2)) <> translateX (1/3) (vrule 2) <> translateX (-1/3) (vrule 2);
sharp2  = fold $ fromVertices ∘ fmap p2 <$> [[(-1/2, -1/2), (1/2, 1/2)], [(-1/2, 1/2), (1/2, -1/2)]];
flat    = translateY (1/2) (vrule 2) <> (scale (1/2) & shearY (1/3)) (arc (-1/4 :: CircleFrac) (1/4 :: CircleFrac));
flat2   = alignR (extrudeRight 0.0625 flat) <> alignL (extrudeLeft 0.0625 flat);
natural = shearY (1/5) (translateY (1/3) (hrule 1) <> translateY (-1/3) (hrule 1)) <> translate (r2 (1/2, 13/30)) (alignT $ vrule 2) <> translate (r2 (-1/2, -13/30)) (alignB $ vrule 2);
space   = strutX 2;

-- Query value is which staff position
-- Zero is midline; pluswise is higher on staff
staff :: (Renderable (Path R2) b) => Double -> QDiagram b R2 (Last Int);
staff l =
  foldMap (flip translateY $ value mempty $ hrule l) [-2..2] <>
  foldMap (\ n ->
           translateY (fromIntegral n/2) $
           value (Last (Just n)) $
           lineColor transparent $
           rect l (1/2)) [-6..6];

-- Synchronized bars
sxbars :: ∀ b . (Renderable (Path R2) b, Backend b R2) => Style -> Time -> [Bar] -> [QDiagram b R2 (Last Rational)];
sxbars s time@(Time _ tb) = join $ fmap ∘ bar' s time ∘ foldr max tb ∘ fmap finesse;

-- Query value is beat number
bar :: (Renderable (Path R2) b, Backend b R2) => Style -> Time -> Bar -> QDiagram b R2 (Last Rational);
bar s time@(Time _ tb) = join $ bar' s time ∘ max tb ∘ finesse;

-- Find shortest note or rest in bar, to scale all appropriately by this
finesse :: Bar -> Int;
finesse (Bar _ _ nrm) =
  let {
    Length n b = foldr min (Length 0 0) (Set.map (\ (NR _ l') -> l') (fold nrm));
  } in b+n;

bar' :: ∀ b . (Renderable (Path R2) b, Backend b R2) => Style -> Time -> Int {- finesse -} -> Bar -> QDiagram b R2 (Last Rational);
bar' s (Time tn tb) b0 (Bar (Clef g0 o0) k nrm) =
  let {
    e = eccentricity s;

    --go :: (PathLike (QDiagram b R2 m), Backend b R2, Monoid' m) => NR -> QDiagram b R2 m;
    go (NR m_p l'@(Length n b)) =
      let {
        -- staff position
        -- rests meant to originate at midline
        p = case m_p of {
              Just (Pitch pc@(PitchClass g a) o) -> fromEnum g - fromEnum g0 + 7*(o-o0);
              _                                  -> 0;
            };

        l = nl l';
      }
      in translateY (fromIntegral p/2) $
         foldr (flip (|||))
         (beside (negate unitX)
          (extrudeRight (1/4) $
           isNothing m_p ? rest s b $
           -- stem down if above midline
           (p > 0 ? rotate (1/2 :: CircleFrac) $ id) $ note s b)
          -- add accidental if pitch class not in key
          (case m_p of {
             Just (Pitch pc@(PitchClass g a) o) | pc ∉ keyPCs k ->
               case a of {
                  2 -> sharp2;
                  1 -> sharp;
                  0 -> natural;
                 -1 -> flat;
                 -2 -> flat2;
                  _ -> rect 1 1;
               } <> space;
             _ -> space;
           } ||| strutX 0.5))
         -- dots
         ((pad (3/2) ∘ scale (1/12)) (fc black unitCircle) <$ [1..n]);

    --positX :: (Monoid' m) => Rational -> QDiagram b R2 m -> QDiagram b R2 m;
    positX p = translateX ∘ (fromRational p *) ∘ (*2^(b0-tb)) ∘ width ∘ go $ NR Nothing (Length 0 0);

    -- temporal locations in bar
    buckets :: (Renderable (Path R2) b, Backend b R2) => QDiagram b R2 (Last Rational);
    buckets =
      lineColor transparent $ hcat $
      ($ rect (width ∘ go $ NR Nothing (Length 0 0)) 10) ∘ value ∘ Last ∘ Just <$>
      takeWhile (< fromIntegral tn) ((/2^(b0-tb)) <$> [0..]);
  }
  in alignL buckets <> (value mempty ∘ fold ∘ Map.mapWithKey positX ∘ fmap alignL) (foldMap go <$> nrm);
