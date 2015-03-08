module DBTuple(DBTuple, toSet, fromSet, filter) where

import Prelude hiding (filter)
import qualified Data.Set as Set
import Data.List (intercalate)

import DBEntry(DBEntry)

newtype DBTuple = DBTupleImpl {
    toSet :: Set.Set DBEntry
} deriving(Eq, Ord)

fromSet :: Set.Set DBEntry -> DBTuple
fromSet = DBTupleImpl

instance Show DBTuple where
    show tp = intercalate "|" $ Set.toList $ Set.map show $ toSet tp

filter :: (DBEntry -> Bool) -> DBTuple -> DBTuple
filter f t = fromSet $ Set.filter f (toSet t)

