module DBRelBody(DBRelBody, toSet, fromSet) where

import qualified Data.Set as Set
import Data.List (intercalate)

import DBTuple(DBTuple)

newtype DBRelBody = DBRelBodyImpl {
    toSet :: Set.Set DBTuple
}

fromSet :: Set.Set DBTuple -> DBRelBody
fromSet = DBRelBodyImpl

instance Show DBRelBody where
    show relb = intercalate "\n" $ Set.toList $ Set.map show $ toSet relb


