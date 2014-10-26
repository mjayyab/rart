let c1 = DBColumn{name="ID", constructor=DBIntT}
let c2 = DBColumn{name="Age", constructor=DBIntT}
let c3 = DBColumn{name="Name", constructor=DBLitT}

let e1 = createDBEntry c1 "0"
let e2 = createDBEntry c2 "10"
let e3 = createDBEntry c3 "Moh"

let t1 = dbtupleFromSet $ Set.fromList [e1, e2, e3]

let e11 = createDBEntry c1 "1"
let e22 = createDBEntry c2 "30"
let e33 = createDBEntry c3 "Jamal"

let t2 = dbtupleFromSet $ Set.fromList [e11, e22, e33]

let rel1 = Relation{headers=dbheadersFromSet (Set.fromList [c1,c2,c3]), tuples=Set.fromList [t1,t2]}
let rel2 = Relation{headers=dbheadersFromSet (Set.fromList [c1,c2,c3]), tuples=Set.fromList [t1]}

let p1 = Predicate EQ' c1 (DBInt 1)
let p1 = Predicate GT' c2 (DBInt 20)

let ca = DBColumn{name="Subject", constructor=DBLitT}
let cb = DBColumn{name="Hours", constructor=DBIntT}

let ea = createDBEntry ca "Math"
let eb = createDBEntry cb "3"

let ta = dbtupleFromSet $ Set.fromList [ea, eb]

let eaa = createDBEntry ca "Science"
let ebb = createDBEntry cb "4"

let taa = dbtupleFromSet $ Set.fromList [eaa, ebb]

let rela = Relation{headers=dbheadersfromSet (Set.fromList [ca, cb]), tuples=Set.fromList [ta,taa]}
