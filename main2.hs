import Debug.Trace
import Data.List

import Text.Printf
import Control.Exception
import System.CPUTime


type Literal = [Char]

type Clause = ([Char], [Literal])

type Arg = Integer

type ArgClause = ([Char], [Arg])

type Relation = (ArgClause, [ArgClause])

type Examples = (PosEx, NegEx)

type PosEx = [Clause]

type NegEx = [Clause]

type BackKnow = [Clause]

type VarMap = [(Integer, Literal)]

backno = [("parent", ["ann", "mary"]), 
            ("parent", ["ann", "tom"]),
            ("parent", ["tom", "eve"]),
            ("parent", ["tom", "ian"]),
            ("female", ["ann"]),
            ("female", ["mary"]),
            ("female", ["eve"])]

backno2 = [("sibling", ["james", "steve"]),
        ("sibling", ["steve", "james"]),
        ("sibling", ["greg", "jess"]),
        ("sibling", ["jess", "greg"]),
        ("sibling", ["bob", "mike"]),
        ("sibling", ["mike", "bob"]),
        ("sibling", ["mark, dave"]),
        ("sibling", ["dave", "mark"]),
        ("parent", ["bob", "james"]),
        ("parent", ["bob", "steve"]),
        ("parent", ["jane", "james"]),
        ("parent", ["jane", "steve"]),
        ("parent", ["mike", "greg"]),
        ("parent", ["mike", "jess"]),
        ("parent", ["sharon", "greg"]),
        ("parent", ["sharon", "jess"]),
        ("parent", ["dave", "jim"]),
        ("parent", ["sarah", "jim"]),
        ("parent", ["mark", "chris"])]


backno5 = [("sibling", ["ron", "ali"]),
            ("sibling", ["ali", "ron"]),
            ("parent", ["bo", "ron"]),
            ("parent", ["joy", "ron"]),
            ("parent", ["bo", "ali"]),
            ("parent", ["joy", "ali"]),
            ("sibling", ["bo", "jill"]),
            ("sibling", ["jill", "bo"]),
            ("parent", ["ben", "sam"]),
            ("parent", ["jill", "sam"]),
            ("sibling", ["beth", "tim"]),
            ("sibling", ["tim", "beth"]),
            ("parent", ["jeff", "beth"]),
            ("parent", ["jeff", "tim"]),
            ("parent", ["kath", "tim"]),
            ("parent", ["kath", "beth"])]

pEx5 = [("cousin", ["ron", "sam"]),
          ("cousin", ["sam", "ron"]),
          ("cousin", ["ali", "sam"])]

nEx5 = [("cousin", ["beth", "sam"]),
          ("cousin", ["tim", "ali"]),
          ("cousin", ["beth", "sam"])]

backno4 = [("a", ["1"]),
        ("a", ["2"]),
        ("a", ["3"]),
        ("b", ["4"]),
        ("b", ["5"]),
        ("b", ["6"]),
        ("c", ["7"]),
        ("c", ["8"]),
        ("c", ["9"])]

pEx4 = [("d", ["1"]),
        ("d", ["2"]),
        ("d", ["3"]),
        ("d", ["7"]),
        ("d", ["8"]),
        ("d", ["9"])]

nEx4 = [("d", ["4"]),
        ("d", ["5"]),
        ("d", ["6"])]

rel0 = (("d", [1]), [("a", [1]), ("c", [1])])
rel00 = (("d", [1]), [])
rel01 = (("d", [1]), [("b", [1])])
rel02 = (("d", [1]), [("a", [1]), ("c", [1])])

pEx2 = [("cousin", ["james", "greg"]),
     ("cousin", ["greg", "james"]),
     ("cousin", ["steve", "greg"]),
     ("cousin", ["greg", "steve"]),
     ("cousin", ["jess", "james"]),
     ("cousin", ["jess", "steve"]),
     ("cousin", ["james", "jess"]),
     ("cousin", ["steve", "jess"]),
     ("cousin", ["chris", "jim"]),
     ("cousin", ["jim", "chris"])]

nEx3 = [("cousin", ["james", "james"]),
        ("cousin", ["sharon", "mike"]),
        ("cousin", ["steve", "james"])]

-- cousin (james, greg) <- parent(bob, james), sibling(bob, mike), parent(mike,
        -- greg)
--
-- 1: james
-- 2: greg
-- 3: bob
-- 4: mike

nEx2 = [("cousin", ["jane", "bob"]),
     ("cousin", ["greg", "jess"]),
     ("cousin", ["mike", "sharon"]),
     ("cousin", ["sharon", "jane"]),
     ("cousin", ["jane", "jess"]),
     ("cousin", ["mike", "mike"]),
     ("cousin", ["sharon", "jess"]),
     ("cousin", ["chris", "jess"]),
     ("cousin", ["chris", "james"]),
     ("cousin", ["chris", "greg"]),
     ("cousin", ["jim", "steve"])]

pEx = [("daughter", ["mary", "ann"]),
       ("daughter", ["eve", "tom"])]

nEx = [("daughter", ["tom", "ann"]),
       ("daughter", ["eve", "ann"])]

rel = (("daughter", [1, 2]), [("female", [1]), ("parent", [2, 1])])
rel2 = (("daugther", [1, 2]), [("female", [1])])
rel3 = (("cousin", [1, 2]), [("parent", [3, 1]), ("sibling", [3,4]),
        ("parent", [4, 2])])
rel4 = (("cousin", [1, 2]), [("parent", [3, 1])])
rel5 = (("cousin", [1, 2]), [("sibling", [1, 2])])
rel6 = (("cousin", [1,2]), [])
rel7 = (("daughter", [1,2]), [])

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
getInVars r = nub $ snd $ fst r

-- get the set of ints used on the right of the relation
getOutVars :: Relation -> [Integer]
getOutVars r = nub $ concat $ map snd (snd r)


-- does this relation cover the positive examples?
coverspos :: Relation -> BackKnow -> [Clause] -> Bool
coverspos r b [] = True
coverspos r b (c:cs) | tryVMaps (possMaps b r c) r b c == False = False
                     | otherwise = coverspos r b cs

coversposc :: Relation -> BackKnow -> [Clause] -> Int
coversposc r b x = coversposc2 r b x 0

coversposc2 :: Relation -> BackKnow -> [Clause] -> Int -> Int
coversposc2 r b [] i = i
coversposc2 r b (c:cs) i | tryVMaps (possMaps b r c) r b c == False = coversposc2 r b cs i
                         | otherwise = coversposc2 r b cs (i+1)

coversnegc :: Relation -> BackKnow -> [Clause] -> Int
coversnegc r b [] = 0
coversnegc r b (c:cs) | tryVMaps (possMaps b r c) r b c == True = (coversnegc r b cs) + 1
                      | otherwise = coversnegc r b cs

-- does this relation cover no negative examples?
coversnoneg :: Relation -> BackKnow -> [Clause] -> Bool
coversnoneg r b [] = True
coversnoneg r b (c:cs) | tryVMaps (possMaps b r c) r b c == True = False
                     | otherwise = coversnoneg r b cs

covers :: Relation -> BackKnow -> Examples -> Bool
covers r b (p, n) = and [(coverspos r b p), (coversnoneg r b n)]

possMaps :: BackKnow -> Relation -> Clause -> [VarMap]
possMaps b r c | diff == [] = vm
                   | otherwise = p4 b diff vm
                   where diff = (getOutVars r) \\ (getInVars r)
                         vm = [zip (getInVars r) (snd c)]

-- utility for p4, messy: needs restructure and rename
p2 :: BackKnow -> Integer -> VarMap -> [VarMap]
p2 b i vm = p3 [(i,y) | y <- (listLits b), not (y `elem` (map snd vm))] vm [] where
            p3 :: [(Integer, Literal)] -> VarMap -> [VarMap] -> [VarMap]
            p3 [] v s = s
            p3 (x:xs) v s = p3 xs v ((v ++ [x]):s)

-- Idea is to take a list of unbound variables in the form of integers and return
-- all the possible mappings of these integers to literals
p4 :: BackKnow -> [Integer] -> [VarMap] -> [VarMap]
p4 b [] y = y
p4 b (x:xs) y = p4 b xs (foldr (++) [] (map (p2 b x) y))

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
useVMap v a = ((fst a), (useVMap2 v (snd a)))

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
              listLits2 (x:xs) s = listLits2 xs (addSet (snd x) s)

-- hoogle this
-- Utility function for listLits
addSet :: (Eq a) => [a] -> [a] -> [a]
addSet [] y = y
addSet (x:xs) y | x `elem` y = addSet xs y
                | otherwise = addSet xs (x:y)

backInfo :: BackKnow -> [([Char], Integer)]
backInfo x = nub (backInfo2 x)

backInfo2 :: BackKnow -> [([Char], Integer)]
backInfo2 [] = []
backInfo2 (x:xs) = (b, (fromIntegral (length c))):(backInfo (dropWhile f xs)) where
                    b = fst x
                    c = snd x
                    f = (\a -> fst a == fst x)

allpos :: Integer -> BackKnow -> [[Literal]]
allpos 1 b = [[x] | x <- listLits b]
allpos n b = [x ++ [y] | x <- (allpos (n-1) b), y <- (listLits b)]

allpos2 :: BackKnow -> BackKnow
allpos2 b = ap2 (backInfo b) [] where
             ap2 [] x = x
             ap2 (x:xs) y = [((fst x), p) | p <- (allpos (snd x) b)] ++ (ap2 xs y)

showb :: BackKnow -> [Char]
showb [] = []
showb (x:xs) = (fst x) ++ (show (snd x)) ++ "\n" ++ (showb xs)

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
mkargcl (c, i) j = [x | x <- endl] where
                    endl = zip (replicate (length (numProd j i)) c) (numProd j i)

combt :: a -> (b, c) -> (a, b, c)
combt x (y, z) = (x, y, z)

listViable :: Relation -> BackKnow -> ([Clause],[Clause]) -> [ArgClause]
--listViable r b (p, n) = [x | x <- (useargs r b), coverspos (conj r x) b p, (coversnegc (conj r x) b n) < (length n)] \\ (snd r)
listViable r b (p, n) = [x | x <- (useargs r b), coverspos (conj r x) b p] \\ (snd r)

-- how many negative examples does this cover?
hScore :: Relation -> BackKnow -> [Clause] -> ArgClause -> Int
hScore r b cs c = coversnegc (conj r c) b cs

-- sort clauses by how many negative examples they cover
--sortAClauses :: Relation -> BackKnow -> Examples -> [ArgClause]
--sortAClauses r b e = sac2 r b e []
--
qSortc :: [ArgClause] -> Relation -> BackKnow -> [Clause] -> [ArgClause]
qSortc [] r b n = []
qSortc (x:xs) r b n = (qSortc higher r b n) ++ [x] ++ (qSortc lower r b n) where
                        lower = filter (\a -> ltc x a r b n) xs
                        higher = filter (\a -> gtc x a r b n) xs

ltc :: ArgClause -> ArgClause -> Relation -> BackKnow -> [Clause] -> Bool
ltc a1 a2 r b n = (hScore r b n a1) < (hScore r b n a2)

gtc :: ArgClause -> ArgClause -> Relation -> BackKnow -> [Clause] -> Bool
gtc a1 a2 r b n = (hScore r b n a1) >= (hScore r b n a2)

improveRel :: Relation -> BackKnow -> Examples -> Relation
improveRel r b e@(p,n) = conj r (head q) where
                        q = qSortc (listViable r b e) r b n

-- hill climb search? holy shit?
findRel :: Relation -> BackKnow -> Examples -> Relation
findRel r b e | covers r b e == True = r
              | otherwise = findRel (improveRel r b e) b e


findRel2 :: Relation -> BackKnow -> Examples -> Relation
findRel2 r b e | covers r b e == True = r
               | otherwise = findRel2 (improveRel r b e) b e


time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v
 
main = do
    putStrLn "Starting..."
    time $ findRel2 rel00 (backno4) (pEx4, nEx4) `seq` return ()
    putStrLn "Done."


main2 = do
    putStrLn "Starting..."
    time $ findRel2 rel6 (backno5) (pEx5, nEx5) `seq` return ()
    putStrLn "Done."

main3 = do
    putStrLn "Starting..."
    time $ findRel2 rel6 (backno5) (pEx5, nEx5) `seq` return ()
    putStrLn "Done."

