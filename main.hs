import Debug.Trace
import Data.List

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
        (True, "parent", ["bob", "james"]),
        (True, "parent", ["bob", "steve"]),
        (True, "parent", ["jane", "james"]),
        (True, "parent", ["jane", "steve"]),
        (True, "parent", ["mike", "greg"]),
        (True, "parent", ["mike", "jess"]),
        (True, "parent", ["sharon", "greg"]),
        (True, "parent", ["sharon", "jess"])]

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
     (True, "cousin", ["steve", "jess"])]

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
     (True, "cousin", ["sharon", "jess"])]

pEx = [(True, "daughter", ["mary", "ann"]),
       (True, "daughter", ["eve", "tom"])]

nEx = [(True, "daughter", ["tom", "ann"]),
       (True, "daughter", ["eve", "ann"])]

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
p2 :: BackKnow -> Integer -> VarMap -> [VarMap]
p2 b i vm = p3 [(i,y) | y <- (listLits b)] vm [] where
            p3 :: [(Integer, Literal)] -> VarMap -> [VarMap] -> [VarMap]
            p3 [] v s = s
            p3 (x:xs) v s = p3 xs v ((v ++ [x]):s)

-- Idea is to take a list of unbound variables in the form of integers and return
-- all the possible mappings of these integers to literals
-- cant believe i figured this out, needs restructure and rename
p4 :: BackKnow -> [Integer] -> [VarMap] -> [VarMap]
p4 b [] y = y
p4 b (x:xs) y = p4 b xs (foldr (++) [] (map (p2 b x) y))

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
numProd i n = [x ++ [y] | x <- (numProd i (n-1)), y <- [1..i]]

mkargcls :: [([Char], Integer)] -> Integer -> [ArgClause]
mkargcls [] i = []
mkargcls (x:xs) i = (mkargcl x i) ++ (mkargcls xs i)

mkargcl :: ([Char], Integer) -> Integer -> [ArgClause]
mkargcl (c, i) j = [(combt True x) | x <- endl] ++ [(combt False x) | x <- endl] where
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
findRel r b e | covers r (completeb b) e == True = r
              | otherwise = findRel (improveRel r (completeb b) e) b e
