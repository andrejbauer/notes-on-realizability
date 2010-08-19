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

draw :: R -> R -> R -> R -> (R -> R) -> Staged (IO ())
draw x1 x2 y1 y2 f = 
    let join l r = do ll <- l; 
                      rr <- r; 
                      return Interval {lower = lower ll, upper = upper rr}
        xs = [(fromIntegral (fst resolution - i) * x1 + fromIntegral i * x2) / fromIntegral (fst resolution) | i <- [0..fst resolution]]
        xs' = zipWith join xs (tail xs)
        transform y = fromIntegral (snd resolution) * (1 - (y - y1) / (y2 - y1))
        ys = map (transform . f) xs'
    in do
      stage <- get_stage
      return $ do
        SDL.init [InitEverything]
        setVideoMode (fst resolution) (snd resolution) 32 []
        s <- getVideoSurface
        approx (drawGraph s ys) stage

drawGraph :: Surface -> [R] -> Staged (IO ())
drawGraph s ys = do
  ys' <- sequence ys
  prec <- get_prec
  return $ do
    SDL.fillRect s Nothing (Pixel 0x00000000)
    mapM_ (\(i, Interval {lower = l, upper = u}) -> 
            let y1 = max 0 ((floor . toFloat) l)
                y2 = min (snd resolution) ((ceiling . toFloat) u)
            in SDL.fillRect s (Just $ Rect i y1 1 (max 0 (y2 - y1))) (Pixel 0x00ffffff))
          (zip [0..] ys')
    SDL.flip s
    handler prec (drawGraph s ys)

handler :: Int -> Staged (IO ()) -> IO ()
handler prec drawAgain = do
    e <- waitEvent
    case e of
        Quit -> SDL.quit
        KeyDown k -> case symKey k of
          SDLK_j -> approx drawAgain (prec_down (prec + 1))
          SDLK_k -> approx drawAgain (prec_down (prec - 1))
          otherwise -> handler prec drawAgain
        otherwise -> handler prec drawAgain

main = approx (draw (-2) 2 (-1) 4 f2) (prec_down 100)
