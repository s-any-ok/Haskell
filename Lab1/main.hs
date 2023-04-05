import Data.List (group)
import Data.List (drop, take, (++))

-- 1.2 a
consecutiveLengths :: Eq a => [a] -> [(a, Int)]
consecutiveLengths x = countInternal x 0 1 []
  where
  countInternal :: Eq a => [a] -> Int -> Int -> [(a, Int)] -> [(a, Int)]
  countInternal [] _ _ res = res
  countInternal [x] _ c res = (res++[(x,c)])
  countInternal (x:x1:xs) i c res 
    | x == x1 = countInternal (x1:xs) (i+1) (c+1) res
    | otherwise = countInternal (x1:xs) (i+1) 1 (res++[(x,c)])

-- 1.2 b
consecutiveLengthsPrel :: String -> [(Char, Int)]
consecutiveLengthsPrel xs = map (\g -> (head g, length g)) (group xs)

-- 2.2 a
shiftLeft :: Int -> [a] -> [a]
shiftLeft n xs 
  | n > len = shiftLeft (n-len) xs
  | otherwise = shift n xs []
  where
    shift :: Int -> [a] -> [a] -> [a]
    shift 0 xs acc = xs ++ acc
    shift _ [] acc = acc
    shift k (x:xs) acc = shift (k-1) xs (acc ++ [x])
    len = length xs

-- 2.2 b
shiftLeftPrel :: Int -> [a] -> [a]
shiftLeftPrel i [] = []
shiftLeftPrel i [x] = [x]
shiftLeftPrel n xs
  | n > len = shiftLeftPrel (n-len) xs
  | otherwise = drop n xs ++ take n xs
  where 
    len = length xs


main = do 
   let input = "aaabbcaadddcccd"
   print(consecutiveLengths input)
   print(consecutiveLengthsPrel input)

   let input2 = [1, 2, 3, 4, 5]
   print(shiftLeft 2 input2)
   print(shiftLeftPrel 2 input2)
