{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude hiding (mapM, foldr);
import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Exception;
import Control.Monad hiding (mapM);
import Data.Array;
import Data.Binary;
import qualified Data.ByteString.Lazy as BS;
import Data.Char;
import Data.Eq.Unicode;
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

data St = St {
  st_l    :: Length,
  st_a    :: Int,
  st_time :: Time,
  st_key  :: Key,
  st_clef :: Clef,
  st_w    :: (P2, P2)
} deriving (Show);

st_view :: (Backend b R2, Monoid' m) => St -> QDiagram b R2 m -> QDiagram b R2 m;
st_view (St { st_w = (P p, P q) }) = view (P p) (q-p);

main = do {
  vp <- newIORef (Piece (array (1, 1) [(1, Time 1 0)]) (array ((1, 1), (1, 1)) [((1, 1), Bar (Clef (PLtr 'B') 4) 0 Map.empty)]));
  stp <- newIORef (St { st_l = Length 0 0, st_a = 0, st_time = Time 1 0, st_key = 0, st_clef = Clef (PLtr 'B') 4, st_w = (p2 (-60, -55), p2 (0, 5)) });
  hintp <- newIORef Nothing; -- hint note
  filename_p <- newIORef "";

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

    prompt :: [Char] -> [Char] -> ([Char] -> IO ()) -> IO ();
    prompt msg hint x = do {
      b <- GTK.hBoxNew False 0;
      e <- GTK.entryNew;
      l <- GTK.labelNew (Just msg);
      GTK.entrySetText e hint;
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
      st_view <$> readIORef stp >>= \ v ->
      tripleA getLast getLast getLast ∘
      flip runQuery (p2 $ join (***) (fromRational ∘ toRational) pos) ∘
      query ∘ snd ∘ adjustDia Cairo o ∘ v ∘ draw <$>
      readIORef vp;

    refresh :: IO ();
    refresh = uncurry (GTK.Rectangle 0 0) <$> GTK.drawableGetSize d >>= flip (GTK.drawWindowInvalidateRect d) True;
  };
  GTK.onDestroy w GTK.mainQuit;
  GTK.widgetSetCanFocus dra True;
  GTK.widgetGrabFocus dra;
  GTK.onExposeRect dra $
  \ r ->
  getBOpts >>= \ o ->
  st_view <$> readIORef stp >>= \ v ->
  readIORef vp >>= GTK.renderWithDrawable d ∘ snd ∘ renderDia Cairo o ∘ v ∘ draw;
  GTK.onButtonPress dra $
  \ ev ->
  getBOpts >>= \ o ->
  uncurry3 (liftA3 (,,)) <$>
  locate (GTK.eventX ev, GTK.eventY ev) >>=
  (maybe (return ()) $ \ ((n, p), g', m) ->
   readIORef  vp >>= \ (Piece ta _) ->
   readIORef stp >>= \ st ->
   modifyIORef vp $ onPieceBars ∘ modifyAt (m, n) $
   case GTK.eventButton ev of {
     GTK.LeftButton  -> \ (Bar clef@(Clef g0 o0) k nrm) ->
                        let {
                          g :: PLtr;
                          g = toEnum $ fromEnum g0 + g';
                          a = list const 0 ∘ Set.toList ∘
                              Set.map (\ (PitchClass _ a) -> a) ∘
                              Set.filter (\ (PitchClass f _) -> f == g) $
                              keyPCs k;
                          pitch = Pitch (PitchClass g (a + st_a st)) ((g' + fromEnum g0) `div` 7 + o0);
                        } in
                        Bar clef k $
                        (Map.insertWith Set.union p ∘ Set.singleton $ NR (Just pitch) (st_l st)) ∘
                        Map.mapWithKey
                        (\ q ->
                         let {
                           Time _ b0 = ta ! n;
                           f | q - p >= (nl ∘ st_l) st * 2^b0 = id
                             | q < p = Set.map $ \ (NR m_p l) -> NR m_p $ isJust m_p && m_p ≠ Just pitch ? l $ min l $ toL ((p - q)/2^b0)
                             | otherwise = Set.filter $ \ (NR m_p _) -> isJust m_p
                             ;
                         } in f) $
                        nrm;
     GTK.RightButton -> \ (Bar clef k nrm) ->
                        Bar clef k $
                        Map.alter
                        ((Just ∘) $ maybe (Set.singleton $ NR Nothing (st_l st)) ∘ join $ \ nrs ->
                         isNothing (find (\ (NR _ l) -> l > st_l st) nrs) ? Set.insert (NR Nothing (st_l st)) $ id) p ∘
                        Map.mapMaybeWithKey
                        (\ q ->
                         let {
                           Time _ b0 = ta ! n;
                           f | q - p >= (nl ∘ st_l) st * 2^b0 = Just
                             | q < p = (Just ∘) $ Set.map $ \ (NR m_p l) -> NR m_p (min l $ toL ((p - q)/2^b0))
                             | q ≡ p = (Just ∘) $ Set.filter $ \ (NR m_p l) -> isNothing m_p && l > st_l st
                             | otherwise = pure Nothing
                             ;
                         } in f) $
                        nrm;
     _               -> id;
   }) >> False <$ refresh;
  GTK.onScroll dra $
  \ ev ->
  case GTK.eventDirection ev of {
     GTK.ScrollDown -> modifyIORef stp $ \ st@St { st_w = (unp2 -> (xl, _), unp2 -> (xr, _)) } -> st { st_w = translateX ((xr - xl)/12) (st_w st) };
     GTK.ScrollUp   -> modifyIORef stp $ \ st@St { st_w = (unp2 -> (xl, _), unp2 -> (xr, _)) } -> st { st_w = translateX ((xl - xr)/12) (st_w st) };
  } >> False <$ refresh;
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
             'C' -> prompt "Clef: " "" $ \ xs ->
                    case xs of {
                      [g, o] | toUpper g ∈ ['A'..'G'], o ∈ ['0'..'9'] -> modifyIORef stp $ \ st -> st { st_clef = Clef (toEnum $ fromEnum (toUpper g) - fromEnum 'A') (toEnum $ fromEnum o - fromEnum '0') };
                      _ -> warn "Invalid clef";
                    };
             'K' -> prompt "Key Signature: " "" $ \ xs ->
                    case xs of {
                      x:xs | (k, _):_ <- reads xs, x ∈ "+-"   -> modifyIORef stp $ \ st -> st { st_key = (case x of { '-' -> negate; '+' -> id; }) k };
                      _ -> warn "Invalid key signature";
                    };
             'T' -> prompt "Time Signature: " "" $ \ xs ->
                    case reads <$> List.splitOn "/" xs of {
                      [(n, _):_, (b', _):_] | b:_ <- (takeWhile ((== b') ∘ (2^)) ∘ dropWhile ((< b') ∘ (2^))) [1..] -> modifyIORef stp $ \ st -> st { st_time = Time n b };
                      _ -> warn "Invalid time signature";
                    };
             'p' -> getPointerLoc >>= print;
             's' -> readIORef stp >>= print;
             'd' -> readIORef vp  >>= print;
             'r' -> readIORef filename_p >>= \ f ->
                    prompt "File to read: "  f $ \ f ->
                    writeIORef filename_p f >>
                    handle ((const :: a -> SomeException -> a) $ warn ("Failed to read file "  ++ f))
                    (decode <$> BS.readFile f >>= writeIORef vp);
             'w' -> readIORef filename_p >>= \ f ->
                    prompt "File to write: " f $ \ f ->
                    writeIORef filename_p f >>
                    handle ((const :: a -> SomeException -> a) $ warn ("Failed to write file " ++ f))
                    (encode <$> readIORef vp >>= BS.writeFile f);
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
                         modifyIORef vp ∘ onPieceBars ∘ ($ Bar (st_clef st) (st_key st) Map.empty) ∘ insertRow1;
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
                              insertCol1 n (Bar (st_clef st) (st_key st) Map.empty)) >>
                             (modifyIORef vp $ onPieceTime $
                              insertElem n (st_time st)));
              _       -> return ();
           }
       ;
    [GTK.Control] | Just kx <- GTK.eventKeyChar ev ->
                      case kx of {
                        '-' -> modifyIORef stp $ \ st -> st { st_w = scale (6/5) (st_w st) };
                        '+' -> modifyIORef stp $ \ st -> st { st_w = scale (5/6) (st_w st) };
                         _  -> return ();
                      }
                  | otherwise ->
                      case GTK.eventKeyName ev of {
                        "Left"  -> modifyIORef stp $ \ st@St { st_w = (unp2 -> (xl, _), unp2 -> (xr, _)) } -> st { st_w = translateX ((xr - xl)/12) (st_w st) };
                        "Right" -> modifyIORef stp $ \ st@St { st_w = (unp2 -> (xl, _), unp2 -> (xr, _)) } -> st { st_w = translateX ((xl - xr)/12) (st_w st) };
                        "Up"    -> modifyIORef stp $ \ st@St { st_w = (unp2 -> (_, yd), unp2 -> (_, yu)) } -> st { st_w = translateY ((yd - yu)/12) (st_w st) };
                        "Down"  -> modifyIORef stp $ \ st@St { st_w = (unp2 -> (_, yd), unp2 -> (_, yu)) } -> st { st_w = translateY ((yu - yd)/12) (st_w st) };
                        _       -> return ();
                      }
                  ;
    _  -> return ();
  } >> False <$ refresh;
  GTK.mainGUI;
};

toL :: Rational -> Length;
toL l =
  let {
    b = head $ dropWhile (\ b ->   1/2^b     >  l) [0..];
    n = last $ takeWhile (\ n -> 3^n/2^(n+b) <= l) [0..];
  } in Length n b;

draw :: Piece -> QDiagram Cairo R2 (Last (BarNumber, Rational {- beat number -}), Last Int {- staff position -}, Last StaffNumber);
draw (Piece ta ba) =
  let {
    go :: Time -> Array StaffNumber Bar -> Array StaffNumber (QDiagram Cairo R2 (Last Rational {- beat number -}));
    go time = uncurry array <<< bounds &&& (uncurry zip <<< id *** Draw.sxbars s time <<< unzip ∘ assocs);

    draw' :: Monoid' m => [QDiagram Cairo R2 m] -> QDiagram Cairo R2 (m, Last Int {- staff position -});
    draw' xs =
      let {
        x = hcat ∘ fmap ((<> value mempty (vrule 4)) ∘ alignR) $ xs;
      } in alignR (flip (,) mempty <$> x) <> alignR ((,) mempty <$> Draw.staff (width x));
  } in vcat ∘ zipWith (fmap ∘ (\ n (p, q) -> (p, q, n <$ q))) (range <<< fst *** fst <<< bounds $ ba) ∘ fmap (draw' ∘ elems ∘ mapWithIx (fmap ∘ fmap ∘ (,))) ∘ rows ∘ colMap (go ∘ (ta !)) $ ba;

f `onPieceBars` Piece ta ba = Piece ta (f ba);
f `onPieceTime` Piece ta ba = Piece (f ta) ba;
