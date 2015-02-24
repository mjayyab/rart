import Relational
import qualified Data.Set as Set

c1 = DBColumn{name="ID", constructor=DBIntT}
c2 = DBColumn{name="Age", constructor=DBIntT}
c3 = DBColumn{name="Name", constructor=DBLitT}

e1 = createDBEntry c1 "0"
e2 = createDBEntry c2 "10"
e3 = createDBEntry c3 "Moh"

t1 = dbtupleFromSet $ Set.fromList [e1, e2, e3]

e11 = createDBEntry c1 "1"
e22 = createDBEntry c2 "30"
e33 = createDBEntry c3 "Jamal"

t2 = dbtupleFromSet $ Set.fromList [e11, e22, e33]

rel1 = Relation{headers=dbheadersFromSet (Set.fromList [c1,c2,c3]), tuples=relBodyFromSet (Set.fromList [t1,t2])}
rel2 = Relation{headers=dbheadersFromSet (Set.fromList [c1,c2,c3]), tuples=relBodyFromSet (Set.fromList [t1])}

p1 = Predicate EQ' c1 (DBInt 1)
p2 = Predicate GT' c2 (DBInt 20)

ca = DBColumn{name="Subject", constructor=DBLitT}
cb = DBColumn{name="Hours", constructor=DBIntT}

ea = createDBEntry ca "Math"
eb = createDBEntry cb "3"

ta = dbtupleFromSet $ Set.fromList [ea, eb]

eaa = createDBEntry ca "Science"
ebb = createDBEntry cb "4"

taa = dbtupleFromSet $ Set.fromList [eaa, ebb]

rela = Relation{headers=dbheadersFromSet (Set.fromList [ca, cb]), tuples=relBodyFromSet (Set.fromList [ta,taa])}

main = do
  return ()