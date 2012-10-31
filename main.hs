import Debug.Trace
import Data.List

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
        ("parent", ["bob", "james"]),
        ("parent", ["bob", "steve"]),
        ("parent", ["jane", "james"]),
        ("parent", ["jane", "steve"]),
        ("parent", ["mike", "greg"]),
        ("parent", ["mike", "jess"]),
        ("parent", ["sharon", "greg"]),
        ("parent", ["sharon", "jess"])]

pEx2 = [("cousin", ["james", "greg"]),
     ("cousin", ["greg", "james"]),
     ("cousin", ["steve", "greg"]),
     ("cousin", ["greg", "steve"]),
     ("cousin", ["jess", "james"]),
     ("cousin", ["jess", "steve"]),
     ("cousin", ["james", "jess"]),
     ("cousin", ["steve", "jess"])]

nEx2 = [("cousin", ["jane", "bob"]),
     ("cousin", ["greg", "jess"]),
     ("cousin", ["mike", "sharon"]),
     ("cousin", ["sharon", "jane"]),
     ("cousin", ["jane", "jess"])]

pEx = [("daughter", ["mary", "ann"]),
       ("daughter", ["eve", "tom"])]

nEx = [("daughter", ["tom", "ann"]),
       ("daughter", ["eve", "ann"])]

rel = (("daughter", [1, 2]), [("female", [1]), ("parent", [2, 1])])
rel2 = (("daugther", [1, 2]), [("female", [1])])
rel3 = (("cousin", [1, 2]), [("parent", [1,3]), ("sibling", [3,4]), ("parent", [4,2])])

getInVars :: Relation -> [Integer]
getInVars r = nub $ snd $ fst r

getOutVars :: Relation -> [Integer]
getOutVars r = nub $ concat $ map snd (snd r)

p2 :: BackKnow -> Integer -> VarMap -> [VarMap]
p2 b i vm = p3 [(i,y) | y <- (listLits b)] vm [] where
            p3 :: [(Integer, Literal)] -> VarMap -> [VarMap] -> [VarMap]
            p3 [] v s = s
            p3 (x:xs) v s = p3 xs v ((v ++ [x]):s)

-- Idea is to take a list of unbound variables in the form of integers and return
-- all the possible mappings of these integers to literals
p4 :: BackKnow -> [Integer] -> [VarMap] -> [VarMap]
p4 b [] y = y
p4 b (x:xs) y = p4 b xs (foldr (++) [] (map (p2 b x) y))

possMappings :: BackKnow -> Relation -> Clause -> [VarMap]
possMappings b r c | diff == [] = vm
                   | otherwise = p4 b diff vm
                   where diff = (getOutVars r) \\ (getInVars r)
                         vm = [zip (getInVars r) (snd c)]

covers2 :: Relation -> BackKnow -> Examples -> Bool
covers2 _ _ ([], _) = True
covers2 r b 

-- Returns true if relation covers all positive examples and none of the negative
-- examples
covers :: Relation -> BackKnow -> Examples -> Bool
covers _ _ ([], _) = True
covers r b (x, (n:ns)) | allIn b (arg2cla r n) == False = covers r b (x, ns)
                       | otherwise = False
covers r b ((e:es), []) | allIn b (arg2cla r e) = covers r b (es, [])
                        | otherwise = False

-- allIn x y returns True if y is a subset of x
allIn :: (Eq a) => [a] -> [a] -> Bool
allIn b [] = True
allIn b (x:xs) | x `elem` b = allIn b xs
               | otherwise = False

-- Returns a mapping of indices to literals               
vMap :: Relation -> Clause -> VarMap
vMap r c = zip (snd (fst rel)) (snd c)

-- Finds the literal for a given index in a varmap
getLit :: VarMap -> Integer -> Literal
getLit [] i | trace ("getLit " ++ (show i)) False = undefined
getLit (v:vs) i | (fst v) == i = snd v
                | otherwise = getLit vs i

-- Transforms a 'conjunction' of arg clauses to real clauses
arg2cla :: Relation -> Clause -> [Clause]
arg2cla r c = arg2cla_ (snd r) c [] where
                arg2cla_ [] c s = s
                arg2cla_ (r2:rs) c s = arg2cla_ rs c ((useVMap (vMap r c) r2):s)

-- Utility function for arg2cla
useVMap :: VarMap -> ArgClause -> Clause
useVMap v a = ((fst a), (useVMap2 v (snd a)))

-- Utility function for arg2cla
useVMap2 :: VarMap -> [Integer] -> [Literal]
useVMap2 v i = useVMap_ v i [] where
                useVMap_ v [] s = (reverse s)
                useVMap_ v (i:ix) s = useVMap_ v ix ((getLit v i):s)

-- Returns the list of literals used in the background knowledge
listLits :: BackKnow -> [Literal]
listLits [] = []
listLits b = listLits2 b [] where
              listLits2 [] s = s
              listLits2 (x:xs) s = listLits2 xs (addSet (snd x) s)

-- Utility function for listLits
addSet :: (Eq a) => [a] -> [a] -> [a]
addSet [] y = y
addSet (x:xs) y | x `elem` y = addSet xs y
                | otherwise = addSet xs (x:y)
