type Literal = [Char]

type Clause = ([Char], [Literal])

type Arg = Integer

type ArgClause = ([Char], [Arg])

type Relation = (ArgClause, [ArgClause])

type Examples = (PosEx, NegEx)

type PosEx = [Clause]

type NegEx = [Clause]

type BackKnow = [Clause]

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
covers _ _ ([], []) = True
covers r b ((e:es), _) = False

varMap :: Relation -> Clause -> [(Integer, Literal)]
varMap r c = zip (snd (fst rel)) (snd c)
