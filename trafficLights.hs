import CodeWorld

singleLight :: Color -> Double -> Picture
singleLight color pos = colored color (translated 0 pos (solidCircle 1))

frame = rectangle 2.5 9.0

trafficLight :: String -> Picture
trafficLight "red" = singleLight black (-3) & singleLight black 0 & singleLight red 3 & frame
trafficLight "yellow" = singleLight black (-3) & singleLight yellow 0 & singleLight black 3 & frame
trafficLight "green" = singleLight green (-3) & singleLight black 0 & singleLight black 3 & frame

trafficController :: Double -> Picture
trafficController t
  | (round(t) `mod` 7) >= 0 && (round(t) `mod` 7) < 3  = trafficLight "red"
  | (round(t) `mod` 7) <= 6 && (round(t) `mod` 7) >= 4  = trafficLight "green"
  | otherwise                = trafficLight "yellow"

main :: IO ()
main = animationOf trafficController
