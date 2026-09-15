module Main where

-- Сортировка слиянием
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort s = let
    (left, right) = splitAt (length s `div` 2) s
    in merge (mergeSort left) (mergeSort right)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do
    putStrLn $ show $ mergeSort [1, 2, 5, 3, 1, 4, 10, 19, 11, 23, 3, 3, 8]
