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

pEx = [("daughter", ["mary", "ann"]),
       ("daughter", ["eve", "tom"])]

nEx = [("daughter", ["tom", "ann"]),
       ("daughter", ["eve", "ann"])]

rel = (("daughter", [1, 2]), [("female", [1]), ("parent", [2, 1])])

covers :: Relation -> BackKnow -> Examples -> Bool
covers _ _ ([], _) = True
covers r b ((e:es), x) | allIn b (arg2cla r e) = covers r b (es, x)
                       | otherwise = False

allIn :: [Clause] -> [Clause] -> Bool
allIn b [] = True
allIn b (x:xs) | x `elem` b = allIn b xs
               | otherwise = False

vMap :: Relation -> Clause -> VarMap
vMap r c = zip (snd (fst rel)) (snd c)

getLit :: VarMap -> Integer -> Literal
getLit (v:vs) i | (fst v) == i = snd v
                | otherwise = getLit vs i

arg2cla :: Relation -> Clause -> [Clause]
arg2cla r c = arg2cla_ (snd r) c [] where
                arg2cla_ [] c s = s
                arg2cla_ (r2:rs) c s = arg2cla_ rs c ((useVMap (vMap r c) r2):s)

useVMap :: VarMap -> ArgClause -> Clause
useVMap v a = ((fst a), (useVMap2 v (snd a)))

useVMap2 :: VarMap -> [Integer] -> [Literal]
useVMap2 v i = useVMap_ v i [] where
                useVMap_ v [] s = (reverse s)
                useVMap_ v (i:ix) s = useVMap_ v ix ((getLit v i):s)



         

