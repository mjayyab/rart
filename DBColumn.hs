module DBColumn where

import Text.Printf (printf)

import DBType(DBType)

data DBColumn = DBColumn { name :: String, constructor :: DBType }

instance Show DBColumn where
    show (DBColumn n c) = printf "%s(%s)" (show c) n

instance Eq DBColumn where
    (DBColumn n1 _) == (DBColumn n2 _) = n1 == n2

instance Ord DBColumn where
    (DBColumn n1 _) `compare` (DBColumn n2 _) = n1 `compare` n2


