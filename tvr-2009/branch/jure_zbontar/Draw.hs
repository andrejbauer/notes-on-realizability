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

join :: R -> R -> R
join l r = do ll <- l
              rr <- r
              return Interval {lower = lower ll, upper = upper rr}

draw :: R -> R -> R -> R -> (R -> R) -> IO ()
draw x1 x2 y1 y2 f = 
    let xpix = (x2 - x1) / fromIntegral (fst resolution)
        ypix = (y2 - y1) / fromIntegral (snd resolution)
        transform x = fromIntegral (snd resolution) - (x - y1) / ypix
        xs = [x1 + fromIntegral i * xpix | i <- [0..fst resolution]]
        ys = map (transform . f) (zipWith join xs (tail xs))
    in do 
        SDL.init [InitEverything]
        setVideoMode (fst resolution) (snd resolution) 32 []
        s <- getVideoSurface
        drawGraph s ys 2

drawGraph :: Surface -> [R] -> Int -> IO ()
drawGraph s ys prec = do
    SDL.fillRect s Nothing (Pixel 0x00000000)
    mapM_ (\(i, a) -> SDL.fillRect s (Just $ Rect i 
                                                  (floor (toFloat (lower a))) 
                                                  1 
                                                  (ceiling (toFloat (upper a - lower a))))
                                   (Pixel 0x00ffffff)) 
          (zip [0..] (map (\r -> approx r (prec_down prec)) ys))
    SDL.flip s
    handler (drawGraph s ys (succ prec))

handler :: IO () -> IO ()
handler drawAgain = do
    e <- waitEvent
    case e of
        Quit -> SDL.quit
        KeyDown k -> drawAgain
        otherwise -> handler drawAgain

main = draw (-2) 2 (-1) 4 f1
