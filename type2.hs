type Baire = Integer -> Integer

u :: ([Integer] -> Integer, Baire) -> Baire

u (a, b) i = x - 1
    where x = head $
              filter (/= 0) $
              [a (i : map b [0..(k-1)]) | k <- [0..]]
