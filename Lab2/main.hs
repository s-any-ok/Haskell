import Data.List (group, map)

-- 1.2 a
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:x1:xs)
    | x == x1 = removeDuplicates (x1:xs)
    | otherwise = x : removeDuplicates (x1:xs)

-- 1.2 b
removeDuplicatesPrel :: (Eq a) => [a] -> [a]
removeDuplicatesPrel xs = foldr (\x acc -> if null acc || x /= head acc then x:acc else acc) [] xs

-- 2.2 a
mergeLists :: [a] -> [a] -> [a]
mergeLists [] _ = []
mergeLists _ [] = []
mergeLists (x:xs) (y:ys)
    | length xs < length ys = x : y : mergeLists xs ys
    | otherwise = x : y : mergeLists xs ys
    where n = min (length xs) (length ys)

-- 2.2 b
mergeListsPrel :: [a] -> [a] -> [a]
mergeListsPrel xs ys = take (2 * n) (interleave xs' ys')
  where
    interleave :: [a] -> [a] -> [a]
    interleave [] ys = ys
    interleave (x:xs) ys = x : interleave ys xs
    n = min (length xs) (length ys)
    xs' = take n xs
    ys' = take n ys


printFuncWithOneArg :: (Show a, Show b) => (a -> b) -> (a -> b) -> a -> IO()
printFuncWithOneArg f1 f2 x = do
    print (x)
    print (f1 x)
    print (f2 x)

printFunctWithTwoArgs :: (Show a, Show b, Show c) => (a -> b -> c) -> (a -> b -> c) -> a -> b -> IO()
printFunctWithTwoArgs f1 f2 a b = do
    print (a, b)
    print (f1 a b)
    print (f2 a b)

main = do 
  printFuncWithOneArg removeDuplicatesPrel removeDuplicates [1,1,1,5,5,3, 1,1,222,222,222,222]
  printFuncWithOneArg removeDuplicatesPrel removeDuplicates [1,1,1,5,5,3, 1,1,222,222,222,222]
  printFuncWithOneArg removeDuplicatesPrel removeDuplicates [1,1,1,5,5,3, 1,1,222,222,222,222]
  print("-----------------------------")
  printFunctWithTwoArgs mergeListsPrel mergeLists "abcde" "123" 
  printFunctWithTwoArgs mergeListsPrel mergeLists "abcde" "1243" 
  printFunctWithTwoArgs mergeListsPrel mergeLists "ab" "123" 