module DBPredicate(DBPredicate(..), evalOp) where

import DBVal(DBVal)
import DBColumn(DBColumn)

evalOp :: Ordering -> DBVal -> DBVal -> Bool
evalOp (EQ) = (==)
evalOp (GT) = (>)
evalOp (LT) = (<)

data DBPredicate = DBPredicate Ordering DBColumn DBVal deriving(Show)


