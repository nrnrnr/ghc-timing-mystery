module Main where

import Data.Char( toLower )
import Data.Function
import Data.List
import qualified Data.Set as Set
import System.Environment

type Set = Set.Set

main :: IO ()
main = do
  (width, depth) <- wd <$> getArgs
  finish width depth
 where wd :: [String] -> (Int, Int)
       wd [] = (10, 5000)
       wd [w] = (read w, 5000)
       wd (w:d:_) = (read w, read d)

       finish width depth = do
             if badness c == 0 then
                 do putStrLn $ "solved { width = " ++ show width ++ ", steps = " ++ show (depth - left) ++ " } -- SOLVED in " ++ show (depth - left) ++ " steps"
                    putStrLn $ "-- " ++ show (vec c)
                    putStrLn $ "-- " ++ sentence (vec c)
             else
                 putStrLn "FAILED"
           where (left, _, c : _) = search width depth (candidate start)

----------------------------------------------------------------

type Vector = [Int]

{-# INLINE contents #-}
contents :: Vector -> String
contents ks =
     andIt [ number k ++ " "
                      ++ [c]
                      ++ if k > 1 then "'s" else ""
           | (k,c) <- ks `zip` ['a'..'z']
           , k > 0
           ]
 where
  andIt [] = "- and -"
  andIt xs = concat [ x ++ ", " | x <- init xs ] ++ "and " ++ last xs

number :: Int -> String
number  1 = "one"
number  2 = "two"
number  3 = "three"
number  4 = "four"
number  5 = "five"
number  6 = "six"
number  7 = "seven"
number  8 = "eight"
number  9 = "nine"
number 10 = "ten"
number 11 = "eleven"
number 12 = "twelve"
number 13 = "thirteen"
number 15 = "fifteen"
number 18 = "eighteen"
number  k | k > 12 && k < 20 = number (k-10) ++ "teen"
number  k | k >= numberLimit = error ("number " ++ show k)
number  k = decs !! (k `div` 10 - 2)
         ++ if k `mod` 10 == 0 then "" else "-" ++ number (k `mod` 10)
 where
  decs = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy"] ++ error ("number " ++ show k)

numberLimit :: Int
numberLimit = 80

----------------------------------------------------------------

oldsentence :: Vector -> String
oldsentence ks =
  "This sentence generated in New York contains " ++ contents ks ++ "."

sentence :: Vector -> String
sentence ks =
  "JP, A while back you posed the challenge of a self-describing message.  This topic came up in my meeting last week.  I am pleased to tell you that this message contains " ++
  contents ks ++
  ".  I hope you like it.  Norman.  P.S. I would love to hear about your solution."

----------------------------------------------------------------

data Candidate = C { vec :: [Int], badness :: Int }
  deriving (Eq, Ord, Show)

candidate :: Vector -> Candidate
candidate xs = C xs (vecBadness xs)

count :: String -> Vector
count s = [ length (filter ((c==) . toLower) s) | c <- ['a'..'z'] ]

vecBadness :: Vector -> Int
vecBadness xs = sum $ map abs $ count (sentence xs) `minus` xs
  where minus = zipWith (-)

mutate :: Candidate -> [Candidate]
mutate c@(C xs _) = stepc c : [C ys (vecBadness ys) | delta <- vectors, let ys = xs `plus` delta]

stepc :: Candidate -> Candidate
stepc (C xs _) = C ys (vecBadness ys)
    where ys = step xs

type Seen = Set Vector

search :: Int -> Int -> Candidate -> (Int, Seen, [Candidate])
search width depth c = go depth Set.empty [c]
  where
    go 0 s cs = (0, s, cs)
    go n s cs =
        if badness (head new) == 0 then
            (n, s, [head new])
        else
            go (pred n) (foldr (Set.insert . vec) s new) new
     where new = concatMap mutate cs & filter (not . seen) & sortBy (compare `on` badness) & take width
           seen c = Set.member (vec c) s


changeable :: [Char]
changeable = sort $ nub $ concatMap number [1..79]

vector :: Char -> [Int]
vector c = [if c == c' then 1 else 0 | c' <- ['a'..'z']]

vectors :: [[Int]]
vectors = map vector changeable

check :: [Int] -> Bool
check xs = count (sentence xs) == xs

step :: Vector -> Vector
step = count . sentence

steps :: [Vector]
steps = step [] : map step steps

nth :: Int -> [a] -> a
nth k xs = head (drop k xs)

oldDeltas :: Int -> Int -> [[Int]]
oldDeltas 0 0 = [[]] -- size len
oldDeltas _ 0 = []
oldDeltas 0 n = map (0 : ) $ oldDeltas 0 (pred n)
oldDeltas size n = (map (0 : ) $ oldDeltas size (pred n)) ++
                (map (1 : ) $ oldDeltas (pred size) (pred n)) ++
                (map ((-1) : ) $ oldDeltas (pred size) (pred n))

--allDeltas :: [[Int]]
--allDeltas = concatMap (flip deltas 26) [1..]

plus :: Vector -> Vector -> Vector
plus = zipWith (+)

combinations :: [[Int]]
combinations = concatMap combos [1..]
  where combos :: Int -> [Vector]
        combos 0 = [[0 | _ <- ['a'..'z']]]
        combos n = [c `plus` d ++ c `plus` map (10*) d | c <- combos (pred n), d <- vectors]

best :: [[Int]] -> [Int]
best = minimumBy (compare `on` vecBadness)

start :: [Int]
start = best $ take 200 $ tail steps


candidatesFrom :: [Int] -> [[Int]]
candidatesFrom start = [ks | deltas <- combinations,
                             let ks = zipWith (+) start deltas,
                            all (>= 0) ks]

solution :: [[Int]] -> Maybe [Int]
solution [] = Nothing
solution (ks:kss) = if vecBadness ks == 0 then Just ks
                    else solution kss

apply :: Int -> (a -> a) -> (a -> a)
apply 0 f x = x
apply n f x = f (apply (pred n) f x)
