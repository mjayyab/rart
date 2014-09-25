
import qualified Data.Set as Set

data DBVal = DBLit String
           | DBInt Int
             deriving(Show)

data DBType = DBLitT | DBIntT deriving(Show)

data DBColumn = DBColumn { name :: String, constructor :: DBType } deriving(Show)

data DBEntry = DBEntry { dBColumn :: DBColumn, value :: DBVal } deriving(Show)

createDBEntry :: DBColumn -> String -> DBEntry
createDBEntry ct@( DBColumn _ DBLitT ) val = DBEntry {dBColumn=ct, value=DBLit val}
createDBEntry ct@( DBColumn _ DBIntT ) val = DBEntry {dBColumn=ct, value=DBInt ((read val)::Int)}

instance Eq DBVal where
    (DBLit str1) == (DBLit str2) = str1 == str2
    (DBInt int1) == (DBInt int2) = int1 == int2

instance Ord DBVal where
    (DBLit str1) `compare` (DBLit str2) = str1 `compare` str2
    (DBInt int1) `compare` (DBInt int2) = int1 `compare` int2

instance Eq DBColumn where
    (DBColumn n1 _) == (DBColumn n2 _) = n1 == n2

instance Ord DBColumn where
    (DBColumn n1 _) `compare` (DBColumn n2 _) = n1 `compare` n2

instance Eq DBEntry where
    (DBEntry c1 v1) == (DBEntry c2 v2) = c1 == c2 && v1 == v2

instance Ord DBEntry where
    (DBEntry c1 v1) `compare` (DBEntry c2 v2) = if c1 == c2 then v1 `compare` v2 else c1 `compare` c2

type DBHeaders = Set.Set DBColumn
type DBTuple = Set.Set DBEntry

data Relation = Relation {headers :: DBHeaders, tuples :: Set.Set DBTuple} deriving(Show)

entryHeader :: DBEntry -> DBColumn
entryHeader en@(DBEntry e_c _) = e_c

entryVal :: DBEntry -> DBVal
entryVal en@(DBEntry _ e_v) = e_v

project :: Relation -> DBHeaders -> Relation
project rel@( Relation hs ts ) projHeaders = Relation {headers = subsetHeaders hs, tuples = subsetTuples ts}
    where subsetHeaders hs = Set.intersection projHeaders hs
          subsetTuples ts = Set.map (Set.filter (\entry -> Set.member (entryHeader entry) projHeaders)) ts

data Operator = EQ' | GT' | LT' deriving(Show)

evalOp :: Operator -> DBVal -> DBVal -> Bool
evalOp (EQ') v1 v2 = v1 == v2
evalOp (GT') v1 v2 = v1 > v2
evalOp (LT') v1 v2 = v1 < v2

data Predicate = Predicate Operator DBColumn DBVal deriving(Show)

restrict :: Relation -> Predicate -> Relation
restrict rel@( Relation hs ts ) (Predicate op col val) = Relation{headers=hs, tuples=filteredTuples}
    where filteredTuples = Set.filter (\t -> any (\e -> ((entryHeader e) == col) && (evalOp op (entryVal e) val)) (Set.toList t)) ts
