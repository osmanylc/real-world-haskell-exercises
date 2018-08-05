-- #1 
import Data.Char (digitToInt)

asInt_fold ('-' : xs) = (-1) * asInt_fold xs
asInt_fold xs = foldl step 0 xs
    where step acc x = 10 * acc + digitToInt x

-- #2 is not well-defined. Will try later.

-- #3
concat' xs = foldr (++) [] xs

-- #4
takeWhile_rec f (x:xs)
    | f x = x : takeWhile_rec f xs
    | otherwise = []
takeWhile_rec _ _ = []

takeWhile_fold f xs = foldr step [] xs
    where step x acc
                | f x = x : acc
                | otherwise = []

-- #5
myGroupBy f xs = foldr step [] xs
    where step x [] = [[x]]
          step x ((i : is) : o)
            | f x i = (x : i : is) : o 
            | otherwise = [x] : (i : is) : o

-- #6
any' f xs = foldr ((||) . f) False xs

