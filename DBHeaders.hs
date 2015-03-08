module DBHeaders (DBHeaders, toSet, fromSet) where

import qualified Data.Set as Set
import Data.List (intercalate)

import DBColumn(DBColumn)

newtype DBHeaders = DBHeadersImpl {
    toSet :: Set.Set DBColumn
}

fromSet :: Set.Set DBColumn -> DBHeaders
fromSet = DBHeadersImpl

instance Show DBHeaders where
    show hs = intercalate "|" $ Set.toList $ Set.map show $ toSet hs

instance Eq DBHeaders where
    hs1 == hs2 = toSet hs1 == toSet hs2


