import Data.List (sortBy, minimumBy)
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
               deriving (Show, Eq)

-- #10
data Point = Point {x :: Double, y :: Double} deriving (Eq, Show)

angleAB (Point xA yA) (Point xB yB) =
    atan2 (yB - yA) (xB - xA)

angleOfTurn a b c = mod' (angleAB a c - angleAB a b) (2*pi)

dirOfTurn a b c 
    | angle > 0 && angle < pi = LEFT
    | angle > pi && angle < (2*pi) = RIGHT
    | otherwise = STRAIGHT
    where angle = angleOfTurn a b c

-- #11
successiveDirs (a:b:c:xs) = (dirOfTurn a b c) : successiveDirs (b:c:xs)
successiveDirs _ = []

-- #12
compareYFirst (Point x1 y1) (Point x2 y2) =
    compare (y1, x1) (y2, x2)

sortByAngle pts = sortBy compareAngle pts
    where a = minimumBy compareYFirst pts
          compareAngle p1 p2 = compare (angleAB a p1) (angleAB a p2)

notRightPts spts = map fst notRightTuples
    where dirs = successiveDirs spts 
          ptDir = zip (tail $ init spts) dirs
          notRightTuples = filter (\(_,d) -> d /= RIGHT) ptDir

convexHull pts = (head spts) : (last spts) : notRightPts spts 
    where spts = sortByAngle pts
