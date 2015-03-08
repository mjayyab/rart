module DBEntry(DBEntry, create, getHeader, getValue) where

import Text.Printf (printf)

import DBVal (DBVal(..))
import DBType (DBType(..))
import DBColumn (DBColumn(..))

data DBEntry = DBEntry { column :: DBColumn, value :: DBVal }

instance Show DBEntry where
    show (DBEntry (DBColumn colname _) val) = printf "%s(%s)" colname (show val)

instance Eq DBEntry where
    (DBEntry c1 v1) == (DBEntry c2 v2) = c1 == c2 && v1 == v2

instance Ord DBEntry where
    (DBEntry c1 v1) `compare` (DBEntry c2 v2) = if c1 == c2 then v1 `compare` v2 else c1 `compare` c2

create :: DBColumn -> String -> DBEntry
create ct@( DBColumn _ DBLitT ) val = DBEntry {column=ct, value=DBLit val}
create ct@( DBColumn _ DBIntT ) val = DBEntry {column=ct, value=DBInt (read val :: Int)}

getHeader :: DBEntry -> DBColumn
getHeader (DBEntry e_c _) = e_c

getValue :: DBEntry -> DBVal
getValue (DBEntry _ e_v) = e_v


