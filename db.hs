
import qualified Data.Set as Set

data DBVal a = DBLit String
             | DBInt Int
             deriving(Show)

data DBType = DBLitT | DBIntT deriving(Show)

data DBColumn = DBColumn { name :: String, constructor :: DBType } deriving(Show)

data DBEntry a = DBEntry { dBColumn :: DBColumn, value :: DBVal a } deriving(Show)

createDBEntry :: DBColumn -> String -> DBEntry b
createDBEntry ct@( DBColumn _ DBLitT ) val = DBEntry {dBColumn=ct, value=DBLit val}
createDBEntry ct@( DBColumn _ DBIntT ) val = DBEntry {dBColumn=ct, value=DBInt ((read val)::Int)}

instance Eq (DBVal a) where
    (DBLit str1) == (DBLit str2) = str1 == str2
    (DBInt int1) == (DBInt int2) = int1 == int2

instance Ord (DBVal a) where
    (DBLit str1) `compare` (DBLit str2) = str1 `compare` str2
    (DBInt int1) `compare` (DBInt int2) = int1 `compare` int2

instance Eq DBColumn where
    (DBColumn n1 _) == (DBColumn n2 _) = n1 == n2

instance Ord DBColumn where
    (DBColumn n1 _) `compare` (DBColumn n2 _) = n1 `compare` n2

instance Eq (DBEntry a) where
    (DBEntry c1 v1) == (DBEntry c2 v2) = v1 == v2 

instance Ord (DBEntry a) where
    (DBEntry _ v1) `compare` (DBEntry _ v2) = v1 `compare` v2 

type DBHeaders = Set.Set DBColumn
type DBTuple a = Set.Set (DBEntry a)

data Relation a = Relation {headers :: DBHeaders, tuples :: Set.Set (DBTuple a)} deriving(Show)

entryHeader :: DBEntry a -> DBColumn
entryHeader en@(DBEntry e_c _) = e_c

entryVal :: DBEntry a -> DBVal a
entryVal en@(DBEntry _ e_v) = e_v

project :: Relation a -> DBHeaders -> Relation a
project rel@( Relation hs ts ) projHeaders = Relation {headers = subsetHeaders hs, tuples = subsetTuples ts}
    where subsetHeaders hs = Set.intersection projHeaders hs
          subsetTuples ts = Set.map (Set.filter (\entry -> Set.member (entryHeader entry) projHeaders)) ts

