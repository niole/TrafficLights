import CodeWorld

singleLight :: Color -> Double -> Picture
singleLight color pos = colored color (translated 0 pos (solidCircle 1))

trafficLight :: Color -> Picture
trafficLight red = singleLight black 0 & singleLight black 3 & singleLight red 6
trafficLight yellow = singleLight black 0 & singleLight yellow 3 & singleLight black 6
trafficLight green = singleLight green 0 & singleLight black 3 & singleLight black 6

trafficController :: Double -> Picture
trafficController t
  | (round(t) `mod` 7) >= 0 && (round(t) `mod` 7) < 3  = trafficLight red
  | (round(t) `mod` 7) <= 6 && (round(t) `mod` 7) >= 4  = trafficLight green
  | otherwise                = trafficLight yellow

main :: IO ()
main = animationOf trafficController
