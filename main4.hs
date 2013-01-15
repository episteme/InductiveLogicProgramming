import Debug.Trace
import Data.List

import Text.Printf
import System.CPUTime


import System.Environment
import System.IO
import System.IO.Error

type Literal = [Char]

type Clause = (Bool, [Char], [Literal])

type Arg = Integer

type ArgClause = (Bool, [Char], [Arg])

type Relation = (ArgClause, [ArgClause])

type Examples = (PosEx, NegEx)

type PosEx = [Clause]

type NegEx = [Clause]

type BackKnow = [Clause]

type VarMap = [(Integer, Literal)]

type TrainEx = [[Char]]

type TrainSet = ([TrainEx],[TrainEx])

backno = [(True, "parent", ["ann", "mary"]), 
            (True, "parent", ["ann", "tom"]),
            (True, "parent", ["tom", "eve"]),
            (True, "parent", ["tom", "ian"]),
            (True, "female", ["ann"]),
            (True, "female", ["mary"]),
            (True, "female", ["eve"])]

backno2 = [(True, "sibling", ["james", "steve"]),
        (True, "sibling", ["steve", "james"]),
        (True, "sibling", ["greg", "jess"]),
        (True, "sibling", ["jess", "greg"]),
        (True, "sibling", ["bob", "mike"]),
        (True, "sibling", ["mike", "bob"]),
        (True, "sibling", ["mark, dave"]),
        (True, "sibling", ["dave", "mark"]),
        (True, "parent", ["bob", "james"]),
        (True, "parent", ["bob", "steve"]),
        (True, "parent", ["jane", "james"]),
        (True, "parent", ["jane", "steve"]),
        (True, "parent", ["mike", "greg"]),
        (True, "parent", ["mike", "jess"]),
        (True, "parent", ["sharon", "greg"]),
        (True, "parent", ["sharon", "jess"]),
        (True, "parent", ["dave", "jim"]),
        (True, "parent", ["sarah", "jim"]),
        (True, "parent", ["mark", "chris"])]


backno5 = [(True, "sibling", ["ron", "ali"]),
          (True, "sibling", ["jane", "mark"]),
          (True, "parent", ["bob", "ron"]),
          (True, "parent", ["jane", "ron"]),
          (True, "parent", ["bob", "ali"]),
          (True, "parent", ["jane", "ali"]),
          (True, "sibling", ["jane", "mark"]),
          (True, "parent", ["mark", "jill"]),
          (True, "parent", ["sharron", "jill"]),
          (True, "sibling", ["sharron", "max"]),
          (True, "parent", ["tim", "max"]),
          (True, "sibling", ["tim", "alan"]),
          (True, "parent", ["ace", "mitt"]),
          (True, "parent", ["flash", "mitt"]),
          (True, "sibling", ["flash", "gordon"])]

pEx5 = [(True, "cousin", ["ron", "jill"]),
        (True, "cousin", ["ali", "jill"]),
        (True, "cousin", ["jill", "ron"]),
        (True, "cousin", ["jill", "ali"])]

nEx5 = [(True, "cousin", ["bob", "ron"]),
        (True, "cousin", ["jane", "mark"]),
        (True, "cousin", ["jane", "ali"]),
        (True, "cousin", ["max", "mark"]),
        (True, "cousin", ["ali", "ron"]),
        (True, "cousin", ["mitt", "ron"])]

backno4 = [(True, "a", ["1"]),
        (True, "a", ["2"]),
        (True, "a", ["3"]),
        (True, "b", ["4"]),
        (True, "b", ["5"]),
        (True, "b", ["6"]),
        (True, "c", ["7"]),
        (True, "c", ["8"]),
        (True, "c", ["9"])]

pEx4 = [(True, "d", ["1"]),
        (True, "d", ["2"]),
        (True, "d", ["3"]),
        (True, "d", ["7"]),
        (True, "d", ["8"]),
        (True, "d", ["9"])]

nEx4 = [(True, "d", ["4"]),
        (True, "d", ["5"]),
        (True, "d", ["6"])]


backno0 = [(True, "a", ["1"]),
        (True, "a", ["2"]),
        (True, "a", ["3"]),
        (True, "a", ["9"]),
        (True, "b", ["4"]),
        (True, "b", ["5"]),
        (True, "b", ["6"]),
        (True, "c", ["2"]),
        (True, "c", ["1"]),
        (True, "c", ["3"]),
        (True, "c", ["7"])]


pEx0 = [(True, "d", ["1"]),
        (True, "d", ["2"]),
        (True, "d", ["3"])]


nEx0 = [(True, "d", ["4"]),
        (True, "d", ["5"]),
        (True, "d", ["6"]),
        (True, "d", ["7"]),
        (True, "d", ["9"])]

rel0 = ((True, "d", [1]), [(False, "a", [1]), (False, "c", [1])])
rel00 = ((True, "d", [1]), [])
rel01 = ((True, "d", [1]), [(False, "b", [1])])
rel02 = ((True, "d", [1]), [(True, "a", [1]), (True, "c", [1])])

pEx2 = [(True, "cousin", ["james", "greg"]),
     (True, "cousin", ["greg", "james"]),
     (True, "cousin", ["steve", "greg"]),
     (True, "cousin", ["greg", "steve"]),
     (True, "cousin", ["jess", "james"]),
     (True, "cousin", ["jess", "steve"]),
     (True, "cousin", ["james", "jess"]),
     (True, "cousin", ["steve", "jess"]),
     (True, "cousin", ["chris", "jim"]),
     (True, "cousin", ["jim", "chris"])]

nEx3 = [(True, "cousin", ["james", "james"]),
        (True, "cousin", ["sharon", "mike"]),
        (True, "cousin", ["steve", "james"])]

-- cousin (james, greg) <- parent(bob, james), sibling(bob, mike), parent(mike,
        -- greg)
--
-- 1: james
-- 2: greg
-- 3: bob
-- 4: mike

nEx2 = [(True, "cousin", ["jane", "bob"]),
     (True, "cousin", ["greg", "jess"]),
     (True, "cousin", ["mike", "sharon"]),
     (True, "cousin", ["sharon", "jane"]),
     (True, "cousin", ["jane", "jess"]),
     (True, "cousin", ["mike", "mike"]),
     (True, "cousin", ["sharon", "jess"]),
     (True, "cousin", ["chris", "jess"]),
     (True, "cousin", ["chris", "james"]),
     (True, "cousin", ["chris", "greg"]),
     (True, "cousin", ["jim", "steve"])]

pEx = [(True, "daughter", ["mary", "ann"]),
       (True, "daughter", ["eve", "tom"])]

nEx = [(True, "daughter", ["tom", "ann"]),
       (True, "daughter", ["eve", "ann"])]


backno8 = [(True, "parent", ["bob", "laura"]),
            (True, "sibling", ["laura", "james"]),
            (True, "sibling", ["james", "laura"]),
            (True, "parent", ["jane", "james"]),
            (True, "sibling", ["jane", "mike"]),
            (True, "sibling", ["mike", "jane"]),
            (True, "parent", ["mike", "bruce"]),
            (True, "parent", ["sarah", "bruce"]),
            (True, "parent", ["bob", "james"]),
            (True, "parent", ["jane", "laura"]),
            (True, "sibling", ["jim", "bob"]),
            (True, "sibling", ["bob", "jim"]),
            (True, "sibling", ["may", "bob"]),
            (True, "male", ["jim"]),
            (True, "male", ["bob"]),
            (True, "male", ["james"]),
            (True, "male", ["mike"]),
            (True, "male", ["bruce"])]

pEx8 = [(True, "uncle", ["mike", "james"]),
        (True, "uncle", ["mike", "laura"]),
        (True, "uncle", ["jim", "laura"]),
        (True, "uncle", ["jim", "james"])]

nEx8 = [(True, "uncle", ["bob", "james"])]

rel8 = ((True, "uncle", [1, 2]), [])
rel9 = ((True, "uncle", [1, 2]), [(True, "parent", [3, 2]), (True, "sibling", [3, 1]), (True, "male", [1])])

rel = ((True, "daughter", [1, 2]), [(True, "female", [1]), (True, "parent", [2, 1])])
rel2 = ((True, "daugther", [1, 2]), [(True, "female", [1])])
rel3 = ((True, "cousin", [1, 2]), [(True, "parent", [3, 1]), (True, "sibling", [3,4]),
        (True, "parent", [4, 2])])
rel4 = ((True, "cousin", [1, 2]), [(True, "parent", [3, 1])])
rel5 = ((True, "cousin", [1, 2]), [(True, "sibling", [1, 2])])
rel6 = ((True, "cousin", [1,2]), [])
rel7 = ((True, "daughter", [1,2]), [])

-- takes a relation and a clause and adds the clause to the right hand side
conj :: Relation -> ArgClause -> Relation
conj (a, b) c = (a, (c:b))

-- hoogle?
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

-- get the set of ints used on the left of the relation
getInVars :: Relation -> [Integer]
getInVars r = nub $ thd3 $ fst r

-- get the set of ints used on the right of the relation
getOutVars :: Relation -> [Integer]
getOutVars r = nub $ concat $ map thd3 (snd r)

-- utility for p4, messy: needs restructure and rename
p2 :: BackKnow -> [Literal] -> Integer -> VarMap -> [VarMap]
p2 b l i vm = p3 [(i,y) | y <- q] vm [] where
            q = filter ((flip notElem) (map snd vm)) l
            p3 [] v s = s
            p3 (x:xs) v s = p3 xs v ((v ++ [x]):s)

-- Idea is to take a list of unbound variables in the form of integers and return
-- all the possible mappings of these integers to literals
-- cant believe i figured this out, needs restructure and rename
p4 :: BackKnow -> [Literal] -> [Integer] -> [VarMap] -> [VarMap]
p4 b l [] y = y
p4 b l (x:xs) y = p4 b l xs (concat (map (p2 b l x) y))

-- does this relation cover the positive examples?
coverspos :: Relation -> BackKnow -> [Literal] -> [Clause] -> Bool
coverspos r b l [] = True
coverspos r b l (c:cs) | tryVMaps (possMaps b l r c) r b c == False = False
                       | otherwise = coverspos r b l cs

coversposc :: Relation -> BackKnow -> [Literal] -> [Clause] -> Int
coversposc r b l x = coversposc2 r b l x 0

coversposc2 :: Relation -> BackKnow -> [Literal] -> [Clause] -> Int -> Int
coversposc2 r b l [] i = i
coversposc2 r b l (c:cs) i | not $ tryVMaps pm r b c = coversposc2 r b l cs i
                           | otherwise = coversposc2 r b l cs (i+1)
                             where pm = possMaps b l r c

uncovered :: Relation -> BackKnow -> [Literal] -> [Clause] -> [Clause]
uncovered _ _ _ [] = []
uncovered r b l (c:cs) | tryVMaps pm r b c = uncovered r b l cs
                       | otherwise = (c : (uncovered r b l cs))
                          where pm = possMaps b l r c

coversnegc :: Relation -> BackKnow -> [Literal] -> [Clause] -> Int
coversnegc r b l [] = 0
coversnegc r b l (c:cs) | tryVMaps pm r b c = (coversnegc r b l cs) + 1
                        | otherwise = coversnegc r b l cs
                           where pm = possMaps b l r c

-- does this relation cover no negative examples?
coversnoneg :: Relation -> BackKnow -> [Literal] -> [Clause] -> Bool
coversnoneg r b l [] = True
coversnoneg r b l (c:cs) | tryVMaps (possMaps b l r c) r b c == True = False
                         | otherwise = coversnoneg r b l cs

-- returns True if x is a subset of y
subsetC :: [Clause] -> [Clause] -> Bool
subsetC [] _ = True
subsetC (c:cs) d | c `elem` d = subsetC cs d
                 | otherwise = False

coverset :: Relation -> BackKnow -> [Literal] -> [Clause] -> [Clause]
coverset r b l n = filter (\a -> tryVMaps (possMaps b l r a) r b a) n

covers :: Relation -> BackKnow -> [Literal] -> Examples -> Bool
covers r b l (p, n) = (coverspos r b l p) && (coversnoneg r b l n)

possMaps :: BackKnow -> [Literal] -> Relation -> Clause -> [VarMap]
possMaps b l r c | diff == [] = vm
                   | otherwise = p4 b l diff vm
                   where diff = (getOutVars r) \\ (getInVars r)
                         vm = [zip (getInVars r) (thd3 c)]

-- do any of these vmappings work?
tryVMaps :: [VarMap] -> Relation -> BackKnow -> Clause -> Bool
tryVMaps [] r b c = False
tryVMaps (x:xs) r b c | tryVMap x r b c == True = True
                      | otherwise = tryVMaps xs r b c

-- does this mapping fit?
tryVMap :: VarMap -> Relation -> BackKnow -> Clause -> Bool
tryVMap v r b c = allIn b (arg2cla v r c)

-- Transforms a 'conjunction' of arg clauses to real clauses
arg2cla :: VarMap -> Relation -> Clause -> [Clause]
arg2cla v r c = arg2cla_ (snd r) c [] where
                arg2cla_ [] c s = s
                arg2cla_ (r2:rs) c s = arg2cla_ rs c ((useVMap v r2):s)

-- Utility function for arg2cla
useVMap :: VarMap -> ArgClause -> Clause
useVMap v a = ((fst3 a), (snd3 a), (useVMap2 v (thd3 a)))

-- Utility function for arg2cla
useVMap2 :: VarMap -> [Integer] -> [Literal]
useVMap2 v i = useVMap_ v i [] where
                useVMap_ v [] s = (reverse s)
                useVMap_ v (i:ix) s = useVMap_ v ix ((getLit v i):s)


-- hoogle this
-- allIn x y returns True if y is a subset of x
allIn :: (Eq a) => [a] -> [a] -> Bool
allIn b [] = True
allIn b (x:xs) | x `elem` b = allIn b xs
               | otherwise = False

-- Finds the literal for a given index in a varmap
getLit :: VarMap -> Integer -> Literal
getLit [] i | trace ("getLit " ++ (show i)) False = undefined
getLit (v:vs) i | (fst v) == i = snd v
                | otherwise = getLit vs i

-- Returns the list of literals used in the background knowledge
listLits :: BackKnow -> [Literal]
listLits [] = []
listLits b = listLits2 b [] where
              listLits2 [] s = s
              listLits2 (x:xs) s = listLits2 xs (addSet (thd3 x) s)

-- Utility function for listLits
addSet :: (Eq a) => [a] -> [a] -> [a]
addSet [] y = y
addSet (x:xs) y | x `elem` y = addSet xs y
                | otherwise = addSet xs (x:y)

backInfo :: BackKnow -> [([Char], Integer)]
backInfo x = nub (backInfo2 x)

backInfo2 :: BackKnow -> [([Char], Integer)]
backInfo2 [] = []
backInfo2 (x:xs) = (b, (fromIntegral (length c))):(backInfo (dropWhile f xs)) 
                   where
                    b = snd3 x
                    c = thd3 x
                    f = (\a -> snd3 a == snd3 x)

allpos :: Integer -> BackKnow -> [[Literal]]
allpos 1 b = [[x] | x <- listLits b]
allpos n b = [x ++ [y] | x <- (allpos (n-1) b), y <- (listLits b)]

allpos2 :: BackKnow -> BackKnow
allpos2 b = ap2 (backInfo b) [] where
              ap2 [] x = x
              ap2 (x:xs) y = [(True, (fst x), p) | p <- (allpos (snd x) b)] ++ (ap2 xs y)

-- generates everything that can possibly be negated in these examples
allpos3 :: [Clause] -> BackKnow -> BackKnow
allpos3 c@(d:ds) b = [(False, (snd3 d), p) | p <- f] \\ (switchallc c) where
                        f = allpos (fromIntegral $ length $ thd3 d) b

allpos4 :: [Clause] -> BackKnow -> BackKnow
allpos4 c@(d:ds) b = [(True, (snd3 d), p) | p <- f] \\ c where
                        f = allpos (fromIntegral $ length $ thd3 d) b
showb :: BackKnow -> [Char]
showb [] = []
showb (x:xs) = (show (fst3 x)) ++ " " ++ (snd3 x) ++ (show (thd3 x)) ++ "\n" ++ (showb xs)

switchc :: Clause -> Clause
switchc (True, c, l) = (False, c, l)
switchc (False, c, l) = (True, c, l)

switchallc :: [Clause] -> [Clause]
switchallc [] = []
switchallc (x:xs) = (switchc x):(switchallc xs)

completeb :: BackKnow -> BackKnow
completeb b = (switchallc ((allpos2 b) \\ b)) ++ b

-- need to generate every useful argclause
useargs :: Relation -> BackKnow -> [ArgClause]
useargs r b = mkargcls b2 (fromIntegral (length argset) + 1) where
                b2 = backInfo b
                argset = nub ((getOutVars r) ++ getInVars r)

numProd :: Integer -> Integer -> [[Integer]]
numProd i 1 = [[x] | x <- [1..i]]
numProd i n = [x ++ [y] | x <- (numProd i (n-1)), y <- [1..i], not (y `elem` x)]

mkargcls :: [([Char], Integer)] -> Integer -> [ArgClause]
mkargcls [] i = []
mkargcls (x:xs) i = (mkargcl x i) ++ (mkargcls xs i)

mkargcl :: ([Char], Integer) -> Integer -> [ArgClause]
mkargcl (c, 1) j = [(combt True x) | x <- endl] ++ [(combt False x) | x <- endl] where
                    endl = zip (replicate (length (numProd (j-1) 1)) c) (numProd (j-1) 1)
mkargcl (c, i) j = [(combt True x) | x <- endl] ++ [(combt False x) | x <- endl] where
                    endl = zip (replicate (length (numProd j i)) c) (numProd j i)

combt :: a -> (b, c) -> (a, b, c)
combt x (y, z) = (x, y, z)

-- how many negative examples does this cover?
hScore :: Relation -> BackKnow -> [Literal] -> [Clause] -> ArgClause -> Int
hScore r b l cs c = coversnegc (conj r c) b l cs

-- need to generate every useful argclause
useargs2 :: Relation -> BackKnow -> [Literal] -> [Clause] -> [[ArgClause]]
useargs2 r b l p = [[x] | x <- useargs r b, coverspos (conj r x) b l p]

useargs3 :: Relation -> BackKnow -> [Literal] -> [Clause] -> [[ArgClause]]
useargs3 r b l p = [[x] | x <- useargs r b]


findAC :: [[ArgClause]] -> Relation -> BackKnow -> [Literal] -> Examples -> [Relation]
findAC [] r b l e = []
findAC (x:xs) r b l e | covers ((fst r), x) b l e = ((fst r), x):(findAC xs r b l e)
                      | otherwise = findAC xs r b l e

showrs :: [Relation] -> String
showrs [] = ""
showrs (x:xs) = (showr x) ++ "\n" ++ (showrs xs)

showr :: Relation -> String
showr r = (snd3 $ fst r) ++ (show $ thd3 $ fst r) ++ " iff " ++ (showc (snd r))
 
showc :: [ArgClause] -> String
showc [] = ""
showc [x] = (show (fst3 x)) ++ " " ++ (snd3 x) ++ (show (thd3 x))
showc (x:xs) = (show (fst3 x)) ++ " " ++ (snd3 x) ++ (show (thd3 x)) ++ " AND " ++ (showc xs)

-- sort relations in increasing number of variables mentioned
sortR :: [Relation] -> [Relation]
sortR = sortBy sortR2

-- creates ordering of relations
sortR2 :: Relation -> Relation -> Ordering
sortR2 r1 r2 | (numVars r1) > (numVars r2) = GT
             | (numVars r1) < (numVars r2) = LT
             | (numVars r1) == (numVars r2) = EQ

-- takes a relation and returns the number of unique variables mentioned
numVars :: Relation -> Int
numVars r = fromIntegral $ maximum ((getInVars r) ++ (getOutVars r))

-- takes a [[ArgClause]] and returns a list of unique [ArgClause]
uniqueACs :: [[ArgClause]] -> [[ArgClause]]
uniqueACs as = uR2 as [] where
                uR2 [] s = s
                uR2 (x:xs) s | elemAC xs x == True = uR2 xs s
                             | otherwise = uR2 xs (x:s)

-- returns True if this ArgClause already appears in some form
elemAC :: [[ArgClause]] -> [ArgClause] -> Bool
elemAC [] a = False
elemAC (c:cs) a | (c \\ a) == [] = True
                | otherwise = elemAC cs a

--
-- START of getting target relation block
--
-- Returns the relation represented by the first line in the parsed file
targetFromFile :: String -> Relation
targetFromFile x = (relFromStr (head (lines x)), [])
 
-- Transform relation string into argclause (Bool, String, [Int])
relFromStr :: String -> ArgClause
relFromStr s | (validTarget s) = (True, head t, nums)
             | otherwise = error ("Input Error: Invalid target relation: " ++ s)
                where 
                  t = words s
                  nums = intsFromStr (t !! 1)

-- Returns True if string can be used in relFromStr
validTarget :: String -> Bool
validTarget s | (length t) < 2 = False -- Target has two parts
              | numeric (t !! 1) = True -- Second part must be a number
              | otherwise = False
                where
                  t = words s
                  numeric [] = True
                  numeric (x:xs) | x `elem` ['0'..'9'] = numeric xs
                                 | otherwise = False

-- Make a list of numbers from 1 up to and including given string
intsFromStr :: String -> [Integer]
intsFromStr s = [1..y] where y = read s::Integer
--
-- END of getting target relation block
-- 

--
-- START of getting background knowledge block
--
-- Gets the background knowledge that is contained within this file
backnoFromFile :: String -> [Clause]
backnoFromFile s = backFromStrs $ takeWhile (\a -> a /= "") (tail $ tail $ lines s)

-- Transform a list of strings into a set of clauses
backFromStrs :: [String] -> [Clause]
backFromStrs [] = []
backFromStrs (x:xs) = (backFromStr x) : (backFromStrs xs)

-- Transforms a single string into a clause
backFromStr :: String -> Clause
backFromStr s | validClause s = doBackFromStr s
              | otherwise = 
                  error ("Input Error: Invalid Background Knowledge: " ++ s)

-- This is where the work is done for backFromStr
doBackFromStr :: String -> Clause
doBackFromStr (x:xs) = (chrToBool x, (head t), (tail t))
                       where
                          t = words xs
                          chrToBool '+' = True
                          chrToBool '-' = False

-- Returns True if string is a valid clause
validClause :: String -> Bool
validClause s | (length t) < 3 = False -- Clause has 3 parts
              | not ((head t) `elem` ["+","-"]) = False -- First part must be +/-
              | otherwise = True
                where
                  t = words s

-- Returns True if this set of clauses is valid
validClauses :: [Clause] -> Bool
validClauses a = vClauses a a

-- This is where the work is done with validClauses
vClauses :: [Clause] -> [Clause] -> Bool
vClauses a [] = True
vClauses a (x:xs) | (clauseInfo x) `elem` backInfo a = vClauses a xs
                  | otherwise = False

-- Convert a clause into it's name and number of arguments
clauseInfo :: Clause -> ([Char], Integer)
clauseInfo c = ((snd3 c), (fromIntegral $ length (thd3 c)))

--
-- END of getting background knowledge block
--
--
-- START of getting positive examples block
--
examplesFromFile :: String -> [Clause]
examplesFromFile s = backFromStrs t
                      where
                        t = tail $ dropWhile f (tail $ dropWhile f (lines s))
                        f a = a /= ""

-- Returns the training examples from a file
trainFromFile :: String -> [TrainEx]
trainFromFile s = map trainFromLine (exampleStrs s)

-- Returns the set of strings that represent examples
exampleStrs :: String -> [String]
exampleStrs s = tail $ dropWhile (\a -> a /= "") (drop 2 w)
                 where w = lines s

-- Returns the traning example from a line
trainFromLine :: String -> TrainEx
trainFromLine s = drop 2 (words s)



--
-- END of getting positive examples block
--
--
-- START of search block
--
-- Returns the entropy of the given examples
entropy :: Floating a => Examples -> a
entropy (a, b) = ent (length a) (length b)

-- Returns the entropy of 2 Ints - used in entropy
ent :: Floating a => Int -> Int -> a
ent a b = - logBase 2 (x / y)
            where
              x = fromIntegral a
              y = x + (fromIntegral b)


gain :: Floating a => Relation -> BackKnow -> Examples -> [Literal] -> ArgClause -> a
gain r b (p, n) l a | x2 == 0 = 0
                    | trace ((show a) ++ " pos_i = " ++ (show x) ++ " pos_i+1 = " ++ (show x2) ++ " e(ci) = " ++ (show (ent x y)) ++ " e(ci+1) = " ++ (show (ent x2 y2)) ++ " gain: " ++ (show t)) otherwise = t
                      where
                       t = (fromIntegral x2) * ((ent x y) - (ent x2 y2))
                       x = coversposc r b l p
                       y = coversnegc r b l n
                       x2 = coversposc (conj r a) b l p
                       y2 = coversnegc (conj r a) b l n

listGains :: Relation -> BackKnow -> Examples -> [Literal] -> [ArgClause] -> [ArgClause]
listGains r b (p, n) l a = filter f a where
                              f x = (gain r b (p, n) l x) > 0

-- Returns the arg clause that would cause the highest information gain
bestGain :: Relation -> BackKnow -> [Literal] -> Examples -> [ArgClause] -> ArgClause
bestGain r b l (p, n) (x:xs) = bestGain2 r b l (p, n) xs (i, x) where i = gain r b (p, n) l x

-- if multiple with same score, picks one with least variables
-- if multiple with same score and no new variables, keeps first
bestGain2 :: (Ord a, Floating a) => Relation -> BackKnow -> [Literal] 
              -> Examples -> [ArgClause] -> (a, ArgClause) -> ArgClause
bestGain2 r b l e [] (i, bs) = bs
--bestGain2 r b e (x:xs) (i, bs) | (trace ((show t) ++ " " ++ (show x))) t > i = bestGain2 r b e xs (t, x)
bestGain2 r b l e (x:xs) (i, bs) | t > i = bestGain2 r b l e xs (t, x)
                                 | t == i = bestGain2 r b l e xs (t, y)
                                 | otherwise = bestGain2 r b l e xs (i, bs)
                                   where
                                     t = gain r b e l x
                                     y = leastNewVars x bs

bestGains :: Relation -> BackKnow -> [Literal] -> Examples -> [ArgClause]
              -> [ArgClause]
bestGains r b l e as =  bestArgs $ listScores r b l e as

listScores :: (Ord a, Floating a) => Relation -> BackKnow -> [Literal]
              -> Examples -> [ArgClause] -> [(a, ArgClause)]
listScores r b l e bs = map f bs where
                        --f x = (trace ((show x) ++ " " ++ (show (gain r b e x)))) ((gain r b e x), x)
                        f x = ((gain r b e l x), x)


maxScore :: (Ord a) => [(a, ArgClause)] -> a
maxScore c = maximum $ map fst c
                                   
maxArgs :: (Ord a) => [(a, ArgClause)] -> [ArgClause]
maxArgs c = map snd $ filter (\x -> (fst x) == (maxScore c)) c

leastNewVarScore :: [ArgClause] -> Integer
leastNewVarScore a = minimum $ map (\x -> maximum (thd3 x)) a

bestArgs :: (Ord a) => [(a, ArgClause)] -> [ArgClause]
bestArgs a = filter (\x -> (maximum (thd3 x)) == (leastNewVarScore t)) t
              where t = maxArgs a

-- returns the argclause that introduces the least variables
leastNewVars :: ArgClause -> ArgClause -> ArgClause
leastNewVars a b | y > x = a
                 | otherwise = b
                  where
                   y = maximum (thd3 b)
                   x = maximum (thd3 a)

improve :: Relation -> BackKnow -> [Literal] -> Examples -> Relation
improve r b l (p, n) | (trace (show r)) n == [] = r
                     | otherwise = improve r1 b1 l (p1, n1) where
                        (r1, b1, (p1, n1), l) = improveR (r, b, (p, n), l)


improve2 :: ([Relation], BackKnow, Examples, [Literal]) -> [Relation]
improve2 ([], _, _, _) = []
--improve2 ((r:rs), b, (p, n)) | (trace (show r)) n == [] = r : (improve2 (rs, b, (p, n)))
improve2 ((r:rs), b, (p, n), l) | n == [] = r : (improve2 (rs, b, (p, n), l))
                                | otherwise = (foldl (++) [] (map improve2 (improveR2 (r, b, (p, n), l))) ++ (improve2 (rs, b, (p,n), l)))



improveR2 :: (Relation, BackKnow, Examples, [Literal]) -> [([Relation], BackKnow, Examples, [Literal])]
improveR2 (r, b, (p, n), l) = [([r1], b, ((coverset r1 b l p), (coverset r1 b l n)), l) | r1 <- rs]
                              where
                                rs = [(conj r x) | x <- (bestGains r b l (p, n) (useargs r b))]

improveR :: (Relation, BackKnow, Examples, [Literal]) -> (Relation, BackKnow, Examples, [Literal])
improveR (r, b, (p, n), l) = (r1, b, (p1, n1), l) 
                            where
                              r1 = conj r (bestGain r b l (p, n) (useargs r b))
                              n1 = coverset r1 b l n
                              p1 = coverset r1 b l p



--
-- END of search block
--


solveFile :: String -> [Relation]
solveFile s = improve2 ([a], b, (c, (allpos4 c b)), d)
                where
                  a = targetFromFile s
                  b = completeb $ backnoFromFile s
                  c = examplesFromFile s
                  d = listLits b


showRel :: Relation -> String
showRel (a, b) = (showArgs [a]) ++ " is implied by " ++ (showArgs b)

showArgs :: [ArgClause] -> String
showArgs [] = ""
showArgs ((b, s, a):[]) | b == True = (show s) ++ (show a)
                        | otherwise = "NOT " ++ (show s) ++ (show a)
showArgs ((b, s, a):xs) | b == True = (showit ++ " AND ") ++ (showArgs xs)
                        | otherwise = ("NOT " ++ showit ++ " AND ") ++ (showArgs xs)
                          where showit = (show s) ++ (show a)

showRels :: [Relation] -> String
showRels [] = ""
showRels (x:xs) = (showRel x) ++ "\n" ++ (showRels xs)

doit :: String -> IO ()
doit file = do
  -- Preprocessing
  contents <- readFile file
  putStrLn $ "Target Relation: "
  putStrLn $ show $ targetFromFile contents
  putStrLn $ "Background Knowledge: "
  putStrLn $ showb $ backnoFromFile contents
  putStrLn $ "Postive Examples: "
  putStrLn $ showb $ examplesFromFile contents
  putStrLn $ "Negative Examples using CWA: "
  putStrLn $ showb $ allpos4 (examplesFromFile contents) (backnoFromFile contents)

  putStrLn $ show $ listLits (backnoFromFile contents)
  putStrLn $ "Begin computation"
  putStrLn $ showRels $ solveFile contents

main = do
  r <- readFile "family.txt"
  print (solveFile r)


time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v
 
main3 = do
    putStrLn "Starting..."
    --time $ findSol rel6 (completeb backno5) (pEx5, nEx5) [] `seq` return ()
    putStrLn "Done."

main4 = do
    putStrLn "Starting..."
    --time $ findSol rel6 (completeb backno2) (pEx2, []) [] `seq` return ()
    putStrLn "Done."

main5 = do
    putStrLn "Starting..."
    --time $ findSol rel8 (completeb backno8) (pEx8, (allpos3 pEx8 backno8)) [] `seq` return ()
    putStrLn "Done."

  
