module Main where

mergeSort :: (Ord a, Show a) => [a] -> (String, [a])
mergeSort = mergeSort' 0
    where
        mergeSort' _ []  = ("", [])
        mergeSort' _ [x] = ("", [x])
        mergeSort' depth xs =
            let
                (left, right)   = splitAt (length xs `div` 2) xs
                (logLeft, sortedLeft) = mergeSort' (depth + 1) left
                (logRight, sortedRight) = mergeSort' (depth + 1) right
                merged   = mergeList sortedLeft sortedRight

                log = splitLog depth xs left right
                    ++ logLeft
                    ++ logRight
                    ++ mergeLog depth sortedLeft sortedRight merged
            in (log, merged)

        mergeList :: Ord a => [a] -> [a] -> [a]
        mergeList [] ys = ys
        mergeList xs [] = xs
        mergeList (x:xs) (y:ys)
            | x <= y    = x : mergeList xs (y:ys)
            | otherwise = y : mergeList (x:xs) ys

        s_indent = "    "
        indent 0 = ""
        indent depth = s_indent ++ indent (depth - 1)

        splitLog depth lst left right =
            indent depth ++ "Split: " ++ show lst ++ " → " ++ show left ++ "  |  " ++ show right ++ "\n"
        mergeLog depth left right lst
            = indent depth ++ "Merge: " ++ show left ++ " + " ++ show right ++ " → " ++ show lst ++ "\n"



main :: IO ()
main = do
    let (trace, sorted) = mergeSort [1, 2, 5, 3, 1, 4, 10, 19, 11, 23, 3, 3, 8]
    putStr trace
    putStrLn $ "Result: " ++ show sorted