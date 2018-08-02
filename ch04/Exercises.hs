import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input,output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed."

          myFunction = transpose' 


-- #1
safeHead (x:_) = Just x
safeHead [] = Nothing

safeTail (_:xs) = Just xs
safeTail [] = Nothing

safeLast xs = if not (null xs)
              then Just (last xs)
              else Nothing

safeInit xs = if not (null xs)
              then Just (init xs)
              else Nothing

-- #2
splitWith f xs = case dropWhile (not . f) xs of
                    [] -> []
                    xs' -> c : splitWith f cs
                        where (c, cs) = span f xs' 

-- #3
firstWord input = unlines $ map (head . words) $ filter (not . null) (lines input)

-- #4
processEmptyLines ls
    | all null ls = []
    | otherwise = map addSpace ls
                  where addSpace "" = " "
                        addSpace s = s

transpose' [] = []
transpose' input =
    let inLines = processEmptyLines $ lines input
        heads = map head inLines
        tails = map tail inLines
    in heads ++ '\n' : (transpose' $ unlines tails)

