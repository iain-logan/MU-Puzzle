module MUPuzzle where

import Data.List
import Parser

-- Recursive data type to represent an MIU string
data MIU = M | I | U deriving (Show, Eq)
data Puzzle = Part MIU Puzzle | End MIU deriving (Eq)
type Path = [Puzzle]

instance Show Puzzle where
    show (End x)     = show x
    show (Part x xs) = show x ++ show xs

-- A handy tree data type for storing different rule applications
data Tree a = Node a ([Tree a]) | Leaf a | UnEval a  deriving (Show)

-- #########################
-- MIU helper functions
-- #########################

-- Takes two MIU strings and sticks them together
add :: Puzzle -> Puzzle -> Puzzle
add (End x) ys     = Part x ys
add (Part x xs) ys = Part x (add xs ys)

listToPuz :: [MIU] -> Puzzle
listToPuz [] = error "Nothing to make list silly"
listToPuz (x:[]) = End x
listToPuz (x:xs) = Part x (listToPuz xs)

-- Constructs a tree from the application of all MIU rules on a given MIU string
makeTree :: Puzzle -> Tree Puzzle
makeTree puz = case (nextStates puz) of
                 [] -> Leaf puz
                 xs -> Node puz (map (UnEval) xs)

growTree :: Tree Puzzle -> Tree Puzzle
growTree (UnEval miu) = makeTree miu
growTree (Node miu xs) = Node miu (map growTree xs)
growTree tree = tree

-- #########################
-- Various searches
-- #########################

-- miuBSearch just a nice wrapper for initial parameters for the actual search (bsearch)
miuBSearch :: Puzzle -> Puzzle -> (Path, Int)
miuBSearch start target = bSearch [[start]] target 0

bSearch :: [Path] -> Puzzle -> Int -> (Path, Int)
bSearch ((miu:ys):xs) target tries | miu == target = (miu:ys, tries)
                                   | otherwise     = bSearch (xs ++ [(x:miu:ys) | x <- (nextStates miu)]) target (tries+1)

-- similar situation as above
miuDSearch :: Puzzle -> Puzzle -> (Path, Int)
miuDSearch start target = dSearch [[start]] target 0

dSearch :: [Path] -> Puzzle -> Int -> (Path, Int)
dSearch ((miu:ys):xs) target tries | miu == target = (miu:ys, tries)
                                   | otherwise     = dSearch ([(x:miu:ys) | x <- (nextStates miu)] ++ xs) target (tries+1)

-- limited depth first search, searchs to depth n and if no solution found evaluate to false
miuDLSearch :: Puzzle -> Puzzle -> Int -> (Path, Int)
miuDLSearch start target depthLim = dLSearch [[start]] target depthLim 0

dLSearch :: [Path] -> Puzzle -> Int -> Int -> (Path, Int)
dLSearch [] _ _ tries = ([], tries)
dLSearch ((miu:ys):xs) target depthLim tries | miu       == target   = (miu:ys, tries)
                                             | length ys >= depthLim = dLSearch xs target depthLim (tries+1)
                                             | otherwise = dLSearch ([(x:miu:ys) | x <- (nextStates miu)] ++ xs) target depthLim (tries+1)

-- iterative deepening search
miuDIDSearch :: Puzzle -> Puzzle -> (Path, Int)
miuDIDSearch start target = dIDSearch start target 1 0

dIDSearch :: Puzzle -> Puzzle -> Int -> Int -> (Path, Int)
dIDSearch start target depth tries = case (dLSearch [[start]] target depth tries) of
                                       ([], dlTries) -> dIDSearch start target (depth+1) (tries+dlTries)
                                       (path, dlTries) -> (path, tries+dlTries)

-- #########################
-- MIU rule functions
-- #########################

nextStates     :: Puzzle -> [Puzzle]
nextStates puz = nub ((rule1 puz) ++ (rule2 puz) ++ (rule3 puz) ++ (rule4 puz))

rule1    :: Puzzle -> [Puzzle]
rule1 (End I)     = [(Part I (End U))]
rule1 (End x)     = []
rule1 (Part x xs) = map (Part x) (rule1 xs)

rule2        :: Puzzle -> [Puzzle]
rule2 (Part M xs) = [Part M (add xs xs)]
rule2 _           = []

rule3    :: Puzzle -> [Puzzle]
rule3 (End x)                       = []
rule3 (Part I (Part I (End I)))     = [End U]
rule3 (Part I (Part I (Part I xs))) = (Part U xs) : map (Part I) (rule3 (Part I (Part I xs)))
rule3 (Part x xs)                   = map (Part x) (rule3 xs)

rule4    :: Puzzle -> [Puzzle]
rule4 (End x)              = []
rule4 (Part x (Part U (End U))) = [End x]
rule4 (Part U (Part U xs)) = xs : map (Part U) (rule4 (Part U xs))
rule4 (Part x xs)          = map (Part x) (rule4 xs)

-- #########################
-- MIU specific parser stuff
-- #########################

i :: Parser MIU
i = (char `sat` ('I'==)) `build` makeI
    where
      makeI char = I

u :: Parser MIU
u = (char `sat` ('U'==)) `build` makeU
    where
      makeU char = U

m :: Parser MIU
m = (char `sat` ('M'==)) `build` makeM
    where
      makeM char = M

miuPuz :: Parser Puzzle
miuPuz = m # many (i `alt` u) `build` makePuz
      where
        makePuz (M, mius) = case mius of
                                   [] -> End M
                                   mius -> Part M (listToPuz mius)

parse :: Parser a -> String -> a
parse p xs = case p xs of
               Nothing -> error "Nothing to parse"
               Just (e, []) -> e
               Just (e, ys) -> error ("Parse error for " ++ show ys)
