module Main where

import Prelude hiding (mapM, foldr);
import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad hiding (mapM);
import Data.Array;
import Data.Foldable;
import Data.Foldable.Unicode;
import Data.IORef;
import qualified Data.List as List;
import qualified Data.List.Split as List;
import Data.Maybe;
import Data.Monoid;
import Data.Map (Map);
import Data.Set (Set);
import qualified Data.Map as Map;
import qualified Data.Set as Set;
import Data.Traversable;
import Diagrams.Backend.Cairo;
import Diagrams.Backend.Cairo.Internal;
import Diagrams.Backend.Gtk;
import Diagrams.Combinators;
import Diagrams.Core;
import Diagrams.Core.Points;
import Diagrams.TwoD.Align;
import Diagrams.TwoD.Combinators;
import Diagrams.TwoD.Size;
import Diagrams.TwoD.Shapes;
import Diagrams.TwoD.Transform;
import Diagrams.TwoD.Types;
import qualified Graphics.UI.Gtk.Gdk.Events as GTK;
import qualified Graphics.UI.Gtk as GTK hiding (eventButton, eventModifier, eventKeyName);
import qualified Graphics.UI.Gtk.Cairo as GTK;
import qualified Draw;
import Types;
import Util;
import Util.Array;

s = Draw.Style 0.8;

clef = Clef (PLtr 'B') 4;

wv = view (p2 (-40, -37)) (r2 (40, 40));
 
data St = St {
  st_l    :: Length,
  st_a    :: Int,
  st_time :: Time,
  st_key  :: Key
} deriving (Show);

main = do {
  vp <- newIORef (Piece (array (1, 1) [(1, Time 1 0)]) (array ((1, 1), (1, 1)) [((1, 1), Bar 0 Map.empty)]));
  φp <- newIORef Nothing; -- phantom note
  stp <- newIORef (St { st_l = Length 0 0, st_a = 0, st_time = Time 1 0, st_key = 0 });
  GTK.initGUI;
  w <- GTK.windowNew;
  box <- GTK.vBoxNew False 0;
  dra <- GTK.drawingAreaNew;
  GTK.boxPackStart box dra GTK.PackGrow 0;
  GTK.containerAdd w box;
  GTK.widgetShowAll w;
  d <- GTK.widgetGetDrawWindow dra;
  let {
    getBOpts =
      (\ (w, h) ->
       CairoOptions "" (Dims (fromIntegral w) (fromIntegral h)) RenderOnly False) <$>
      GTK.drawableGetSize d;

    getPointerLoc :: IO (Maybe (BarNumber, Rational), Maybe Int, Maybe StaffNumber);
    getPointerLoc =
      GTK.drawWindowGetPointerPos d >>= \ (_, x, y, mods) ->
      locate (x, y);

    prompt :: [Char] -> ([Char] -> IO ()) -> IO ();
    prompt msg x = do {
      b <- GTK.hBoxNew False 0;
      e <- GTK.entryNew;
      l <- GTK.labelNew (Just msg);
      GTK.boxPackStart b l GTK.PackNatural 0;
      GTK.boxPackEnd   b e GTK.PackGrow    0;
      GTK.boxPackEnd box b GTK.PackNatural 0;
      GTK.widgetShowAll box;
      GTK.editableSetEditable e True;
      GTK.widgetGrabFocus e;
      () <$ GTK.onEntryActivate e (GTK.entryGetText e >>= x >> GTK.containerRemove box b >> GTK.widgetGrabFocus dra);
    };

    warn :: [Char] -> IO ();
    warn msg = do {
      l <- GTK.labelNew (Just msg);
      lp <- newIORef (Just l);
      GTK.boxPackEnd box l GTK.PackNatural 0;
      GTK.widgetShowAll box;
      () <$ mapM ($ \ _ -> readIORef lp >>= maybe (return ()) (GTK.containerRemove box) >> modifyIORef lp (const Nothing) >> return False) [GTK.onKeyPress w, GTK.onButtonPress w];
    };

    locate :: Real a => (a, a) -> IO (Maybe (BarNumber, Rational), Maybe Int, Maybe StaffNumber);
    locate pos =
      getBOpts >>= \ o ->
      tripleA getLast getLast getLast ∘
      flip runQuery (p2 $ join (***) (fromRational ∘ toRational) pos) ∘
      query ∘ snd ∘ adjustDia Cairo o ∘ wv ∘ draw <$>
      readIORef vp;
  };
  GTK.onDestroy w GTK.mainQuit;
  GTK.widgetSetCanFocus dra True;
  GTK.widgetGrabFocus dra;
  GTK.onExposeRect dra $
  \ r ->
  getBOpts >>= \ o ->
  readIORef vp >>= GTK.renderWithDrawable d ∘ snd ∘ renderDia Cairo o ∘ wv ∘ draw;
  GTK.onButtonPress dra $
  \ ev ->
  getBOpts >>= \ o ->
  uncurry3 (liftA3 (,,)) <$>
  locate (GTK.eventX ev, GTK.eventY ev) >>=
  (maybe (return ()) $ \ ((n, p), g', m) ->
   readIORef stp >>= \ st ->
   modifyIORef vp $ onPieceBars ∘ modifyAt (m, n) $
   case GTK.eventButton ev of {
     GTK.LeftButton ->  \ (Bar k nrm) ->
                        Bar k $
                        (Map.insertWith Set.union p ∘ Set.singleton $
                         NR (Just (let {
                                     Clef g0 o0 = clef;
                                     g :: PLtr;
                                     g = toEnum $ fromEnum g0 + g';
                                     a = list const 0 ∘ Set.toList ∘
                                         Set.map (\ (PitchClass _ a) -> a) ∘
                                         Set.filter (\ (PitchClass f _) -> f == g) $
                                         keyPCs k;
                                   }
                                   in Pitch (PitchClass g (a + st_a st)) ((g' + fromEnum g0) `div` 8 + o0))) (st_l st)) $
                        Map.adjust (Set.filter $ \ (NR m_p _) -> isJust m_p) p $
                        nrm;
     GTK.RightButton -> \ (Bar k nrm) ->
                        Bar k $
                        (Map.insert p ∘ Set.singleton $
                         NR Nothing (st_l st)) nrm;
     _               -> id;
   }) >>
  uncurry (GTK.Rectangle 0 0) <$> GTK.drawableGetSize d >>=
  flip (GTK.drawWindowInvalidateRect d) True >> return False;
  GTK.onKeyPress dra $
  \ ev ->
  case GTK.eventModifier ev List.\\ [GTK.Shift, GTK.Lock, GTK.Button1, GTK.Button2, GTK.Button3, GTK.Button4, GTK.Button5, GTK.Alt2] of {
    [] | Just kx <- GTK.eventKeyChar ev ->
           case kx of {
              _  | kx ∈ ['0'..'9']
                 -> modifyIORef stp $ \ st                          -> st { st_l = Length 0 $ fromEnum kx - fromEnum '0' };
             '.' -> modifyIORef stp $ \ st@St { st_l = Length n b } -> st { st_l = Length (n+1) b };
             '+' -> modifyIORef stp $ \ st@St { st_a = a          } -> st { st_a = a + 1 };
             '-' -> modifyIORef stp $ \ st@St { st_a = a          } -> st { st_a = a - 1 };
             'K' -> prompt "Key Signature: " $ \ xs ->
                    case xs of {
                      x:xs | (k, _):_ <- reads xs, x ∈ "+-"   -> modifyIORef stp $ \ st -> st { st_key = (case x of { '-' -> negate; '+' -> id; }) k };
                      _ -> warn "Invalid key signature";
                    };
             'T' -> prompt "Time Signature: " $ \ xs ->
                    case reads <$> List.splitOn "/" xs of {
                      [(n, _):_, (b', _):_] | b:_ <- (takeWhile ((== b') ∘ (2^)) ∘ dropWhile ((< b') ∘ (2^))) [1..] -> modifyIORef stp $ \ st -> st { st_time = Time n b };
                      _ -> warn "Invalid time signature";
                    };
             'p' -> getPointerLoc >>= print;
             's' -> readIORef stp >>= print;
             'd' -> readIORef vp  >>= print;
              _  -> return ();
           }
       | otherwise ->
           case GTK.eventKeyName ev of {
             "Delete" | GTK.Shift ∈ GTK.eventModifier ev
                      -> þrd3 <$> getPointerLoc >>=
                         maybe (return ())
                         (modifyIORef vp ∘ onPieceBars ∘ deleteRow);
             "Insert" | GTK.Shift ∈ GTK.eventModifier ev
                      -> readIORef stp >>= \ st ->
                         þrd3 <$> getPointerLoc >>=
                         modifyIORef vp ∘ onPieceBars ∘ ($ Bar (st_key st) Map.empty) ∘ insertRow1;
             "Delete" -> fst3 & fmap fst <$> getPointerLoc >>=
                         maybe (return ())
                         (modifyIORef vp ∘ onPieceBars ∘ deleteCol);
             "Insert" -> readIORef stp >>= \ st ->
                         fst3 <$> getPointerLoc >>=
                         (\ m_np ->
                          readIORef vp >>= \ (Piece ta _) ->
                          let {
                            n :: Maybe BarNumber;
                            n = m_np >>= \ (n, p) -> ta ?! n >>= \ (Time tn _) -> Just $ n + round (p/fromIntegral tn);
                          }
                          in (modifyIORef vp $ onPieceBars $
                              insertCol1 n (Bar (st_key st) Map.empty)) >>
                             (modifyIORef vp $ onPieceTime $
                              insertElem n (st_time st)));
              _       -> return ();
           }
       ;
    _  -> return ();
  } >>
  uncurry (GTK.Rectangle 0 0) <$> GTK.drawableGetSize d >>=
  flip (GTK.drawWindowInvalidateRect d) True >> return False;
  GTK.mainGUI;
};

draw :: Piece -> QDiagram Cairo R2 (Last (BarNumber, Rational {- beat number -}), Last Int {- staff position -}, Last StaffNumber {- staff number -});
draw (Piece ta ba) =
  let {
    go :: Time -> Array StaffNumber Bar -> Array StaffNumber (QDiagram Cairo R2 (Last Rational {- beat number -}));
    go time = uncurry array <<< bounds &&& (uncurry zip <<< id *** Draw.sxbars s clef time <<< unzip ∘ assocs);

    draw' :: Monoid' m => [QDiagram Cairo R2 m] -> QDiagram Cairo R2 (m, Last Int {- staff position -});
    draw' xs =
      let {
        x = hcat ∘ fmap ((<> value mempty (vrule 4)) ∘ alignR) $ xs;
      } in alignR (flip (,) mempty <$> x) <> alignR ((,) mempty <$> Draw.staff (width x));
  } in vcat ∘ zipWith (fmap ∘ (\ n (p, q) -> (p, q, n <$ q))) (range <<< fst *** fst <<< bounds $ ba) ∘ fmap (draw' ∘ elems ∘ mapWithIx (fmap ∘ fmap ∘ (,))) ∘ rows ∘ colMap (go ∘ (ta !)) $ ba;

f `onPieceBars` Piece ta ba = Piece ta (f ba);
f `onPieceTime` Piece ta ba = Piece (f ta) ba;
