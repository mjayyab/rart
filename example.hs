let c1 = DBColumn{name="ID", constructor=DBIntT}
let c2 = DBColumn{name="Age", constructor=DBIntT}
let c3 = DBColumn{name="Name", constructor=DBLitT}

let e1 = createDBEntry c1 "0"
let e2 = createDBEntry c2 "10"
let e3 = createDBEntry c3 "Moh"

let t1 = Set.fromList [e1, e2, e3]

let e11 = createDBEntry c1 "1"
let e22 = createDBEntry c2 "30"
let e33 = createDBEntry c3 "Jamal"

let t2 = Set.fromList [e11, e22, e33]

let rel = Relation{headers=Set.fromList [c1,c2,c3], tuples=Set.fromList [t1,t2]}

let p1 = Predicate EQ' c1 (DBInt 1)
let p1 = Predicate GT' c2 (DBInt 20)


