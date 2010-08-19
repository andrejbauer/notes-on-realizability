import Reals
import Dyadic
import Interval
import Staged
import Graphics.UI.SDL as SDL

type R = RealNum Dyadic

resolution :: (Int, Int)
resolution = (800, 600)

f1 :: R -> R
f1 x = x^2

f2 :: R -> R
f2 x = 1 / x 

white = Pixel 0x00ffffff
black = Pixel 0x00000000

join l r = do ll <- l;
              rr <- r;
              return Interval {lower = lower ll, upper = upper rr}

draw n x1 x2 y1 y2 f = 
  let transform r a b x = fromIntegral r * (1 - (x - a) / (b - a))
      rec s 0 x1' x2' = 
        let x_real = join x1' x2' in do 
          x <- transform (fst resolution) x2 x1 x_real
          y <- transform (snd resolution) y1 y2 (f x_real)
          return $ 
            let xFrom = (floor . toFloat . lower) x
                xTo = (ceiling . toFloat . upper) x
                yFrom = max 0 ((floor . toFloat . lower) y)
                yTo = min (snd resolution) ((ceiling . toFloat . upper) y)
            in SDL.fillRect s (Just $ Rect xFrom yFrom (xTo - xFrom) (max 0 (yTo - yFrom))) white
      rec s depth x1 x2 =
        let xMid = (x1 + x2) / 2 in do 
          o1 <- rec s (depth - 1) x1 xMid
          o2 <- rec s (depth - 1) xMid x2
          return $ do o1; o2
  in do
    stage <- get_stage
    return $ do
      SDL.init [InitEverything]
      setVideoMode (fst resolution) (snd resolution) 32 []
      s <- getVideoSurface
      approx (rec s n x1 x2) stage
      SDL.flip s
      handler

handler :: IO ()
handler = do
  e <- waitEvent
  case e of
    Quit -> SDL.quit
    otherwise -> handler

main = approx (draw 10 (-2) 2 (-1) 4 f1) (prec_down 20)
