import Data.List (sortBy)
import Data.Fixed (mod')

-- #1 & #2
myLength :: Num a => [b] -> a
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

-- #3
myMean :: Fractional a => [a] -> a
myMean [] = 0
myMean xs = (mySum xs) / (myLength xs)
    where mySum (x:xs) = x + mySum xs
          mySum [] = 0

-- #4
toPalindrome xs = xs ++ (myReverse xs)
    where myReverse (x:xs) = (myReverse xs) ++ [x]
          myReverse [] = []

-- #5
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = (x == last xs)
                   && (isPalindrome $ init xs)
-- #6
lengthOrdering xs ds = 
    compare (myLength xs) (myLength ds)

lengthSort xs = sortBy lengthOrdering xs

-- #7
intersperse :: a -> [[a]] -> [a]
intersperse sep [] = []
intersperse sep [x] = x
intersperse sep (x:xs) = 
    x ++ sep : (intersperse sep xs)

-- #8
data Tree a = Node a (Tree a) (Tree a)
            | Empty

myTreeHeight (Node a left right) = 
    1 + max (myTreeHeight left) (myTreeHeight right)
myTreeHeight Empty = 0

-- #9
data Direction = LEFT 
               | RIGHT
               | STRAIGHT 
               deriving (Show)

-- #10
data Point = Point {x :: Double, y :: Double} deriving (Eq, Show)

angleOfTurn (Point x1 y1) (Point x2 y2) (Point x3 y3) = 
    mod' (angleAC - angleAB) (2*pi)
    where angleAC = atan2 (y3 - y1) (x3 - x1)
          angleAB = atan2 (y2 - y1) (x2 - x1)

dirOfTurn a b c 
    | angle > 0 && angle < pi = LEFT
    | angle > pi && angle < (2*pi) = RIGHT
    | otherwise = STRAIGHT
    where angle = angleOfTurn a b c

-- #11
successiveDirs (a:b:c:xs) = (dirOfTurn a b c) : successiveDirs (b:c:xs)
successiveDirs _ = []

