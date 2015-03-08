module DBVal where

data DBVal = DBLit String
           | DBInt Int

instance Show DBVal where
    show (DBLit s) = s
    show (DBInt i) = show i

instance Eq DBVal where
    (DBLit str1) == (DBLit str2) = str1 == str2
    (DBInt int1) == (DBInt int2) = int1 == int2

instance Ord DBVal where
    (DBLit str1) `compare` (DBLit str2) = str1 `compare` str2
    (DBInt int1) `compare` (DBInt int2) = int1 `compare` int2

