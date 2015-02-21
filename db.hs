module Relational where

import qualified Data.Set as Set
import Text.Printf (printf)
import Data.List (intercalate)

data DBVal = DBLit String
           | DBInt Int

instance Show DBVal where
    show (DBLit s) = s
    show (DBInt i) = show i

data DBType = DBLitT | DBIntT deriving(Show)

data DBColumn = DBColumn { name :: String, constructor :: DBType }

instance Show DBColumn where
    show (DBColumn n c) = printf "%s(%s)" (show c) n

data DBEntry = DBEntry { dBColumn :: DBColumn, value :: DBVal }

instance Show DBEntry where
    show (DBEntry (DBColumn colname _) val) = printf "%s(%s)" colname (show val)

createDBEntry :: DBColumn -> String -> DBEntry
createDBEntry ct@( DBColumn _ DBLitT ) val = DBEntry {dBColumn=ct, value=DBLit val}
createDBEntry ct@( DBColumn _ DBIntT ) val = DBEntry {dBColumn=ct, value=DBInt (read val :: Int)}

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

newtype DBHeaders = DBHeaders {
    toSet :: Set.Set DBColumn
}

dbheadersFromSet :: Set.Set DBColumn -> DBHeaders
dbheadersFromSet = DBHeaders

instance Show DBHeaders where
    show hs = intercalate "|" $ Set.toList $ Set.map show $ toSet hs

instance Eq DBHeaders where
    hs1 == hs2 = toSet hs1 == toSet hs2

newtype DBTuple = DBTuple {
    dbtupleToSet :: Set.Set DBEntry
} deriving(Eq, Ord)

instance Show DBTuple where
    show tp = intercalate "|" $ Set.toList $ Set.map show $ dbtupleToSet tp

dbtupleFromSet :: Set.Set DBEntry -> DBTuple
dbtupleFromSet = DBTuple

newtype RelationBody = RelationBody {
    relBodyToSet :: Set.Set DBTuple
}

relBodyFromSet :: Set.Set DBTuple -> RelationBody
relBodyFromSet = RelationBody

instance Show RelationBody where
    show relb = intercalate "\n" $ Set.toList $ Set.map show $ relBodyToSet relb

data Relation = Relation {headers :: DBHeaders, tuples :: RelationBody}

instance Show Relation where
    show (Relation hs ts) = intercalate "\n" [show hs, show ts]

entryHeader :: DBEntry -> DBColumn
entryHeader (DBEntry e_c _) = e_c

entryVal :: DBEntry -> DBVal
entryVal (DBEntry _ e_v) = e_v

filterDbTuple :: (DBEntry -> Bool) -> DBTuple -> DBTuple
filterDbTuple f t = dbtupleFromSet $ Set.filter f (dbtupleToSet t)

data Operator = EQ' | GT' | LT' deriving(Show)

evalOp :: Operator -> DBVal -> DBVal -> Bool
evalOp (EQ') v1 v2 = v1 == v2
evalOp (GT') v1 v2 = v1 > v2
evalOp (LT') v1 v2 = v1 < v2

data Predicate = Predicate Operator DBColumn DBVal deriving(Show)

newtype Evaluator a = Ev (Either String a) deriving (Show)
instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
          Left msg -> Ev (Left msg)
          Right v -> k v
    return v = Ev (Right v)
    fail msg = Ev (Left msg)

project :: DBHeaders -> Relation -> Evaluator Relation
project projHeaders ( Relation hs ts ) = return Relation {headers = subsetHeaders hs, tuples = subsetTuples ts}
    where subsetHeaders hrs = dbheadersFromSet $ Set.intersection (toSet projHeaders) (toSet hrs)
          subsetTuples tls = relBodyFromSet $
                            Set.map (filterDbTuple
                                        (\entry -> Set.member (entryHeader entry) (toSet projHeaders))
                                    )
                                    (relBodyToSet tls)

restrict :: Predicate -> Relation -> Evaluator Relation
restrict (Predicate op col val) (Relation hs ts) = return Relation{headers=hs, tuples=filteredTuples}
    where filteredTuples = relBodyFromSet $ Set.filter (\t -> any (\e -> (entryHeader e == col) && evalOp op (entryVal e) val) (Set.toList $ dbtupleToSet t)) (relBodyToSet ts)

product :: Relation -> Relation -> Evaluator Relation
product (Relation hs1 ts1) (Relation hs2 ts2) =
    if Set.null $ Set.intersection (toSet hs1) (toSet hs2)
        then return Relation{headers= dbheadersFromSet $ Set.union (toSet hs1) (toSet hs2), tuples=cartesianProd ts1 ts2}
        else fail "Cannot perform product because of shared headers"
    where cartesianProd tls1 tls2 = relBodyFromSet $ Set.fromList [dbtupleFromSet (dbtupleToSet i `Set.union` dbtupleToSet j) | i <- Set.toList $ relBodyToSet tls1, j <- Set.toList $ relBodyToSet tls2]

union :: Relation -> Relation -> Evaluator Relation
union (Relation hs1 ts1) (Relation hs2 ts2) =
    if hs1 == hs2
        then return Relation{headers=hs1, tuples= relBodyFromSet (relBodyToSet ts1 `Set.union` relBodyToSet ts2)}
        else fail "Cannot perfrom union on relations with mismatching headers"

intersection :: Relation -> Relation -> Evaluator Relation
intersection (Relation hs1 ts1) (Relation hs2 ts2) =
    if hs1 == hs2
        then return Relation{headers=hs1, tuples= relBodyFromSet (Set.intersection (relBodyToSet ts1) (relBodyToSet ts2))}
        else fail "Cannot perfrom intersection on relations with mismatching headers"

difference :: Relation -> Relation -> Evaluator Relation
difference (Relation hs1 ts1) (Relation hs2 ts2) =
    if hs1 == hs2
        then return Relation{headers=hs1, tuples= relBodyFromSet (Set.difference (relBodyToSet ts1) (relBodyToSet ts2))}
        else fail "Cannot perfrom difference on relations with mismatching headers"

