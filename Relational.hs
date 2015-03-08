{-# LANGUAGE RankNTypes #-}

module Relational(RelValue, Expr, eval) where

import Data.Set as Set

import DBRelation as DBR (DBRelation(..), DBEvaluator)
import DBHeaders as DBH (DBHeaders, toSet, fromSet)
import DBRelBody as DBB (DBRelBody, toSet, fromSet)
import DBPredicate as DBP (DBPredicate(DBPredicate), evalOp)
import DBTuple as DBT (toSet, fromSet, filter)
import DBEntry as DBE (getHeader, getValue)

project :: DBHeaders -> DBR.DBRelation -> DBR.DBEvaluator DBR.DBRelation
project projHeaders ( DBR.DBRelation hs ts ) = 
    return DBR.DBRelation {
            DBR.headers = subsetHeaders hs, 
            DBR.tuples  = subsetTuples ts
    } 
    where subsetHeaders hrs = DBH.fromSet $ 
                                Set.intersection 
                                    (DBH.toSet projHeaders) 
                                    (DBH.toSet hrs)
          subsetTuples tls  = DBB.fromSet $
                                Set.map 
                                    (DBT.filter
                                        (\entry -> Set.member (DBE.getHeader entry) 
                                                              (DBH.toSet projHeaders)))
                                    (DBB.toSet tls)

restrict :: DBPredicate -> DBR.DBRelation -> DBR.DBEvaluator DBR.DBRelation
restrict (DBPredicate op col val) (DBR.DBRelation hs ts) = 
    return DBR.DBRelation {
        DBR.headers = hs, 
        DBR.tuples = filteredTuples
    }
    where filteredTuples = DBB.fromSet $ 
                            Set.filter 
                                (\t -> any 
                                        (\e -> (DBE.getHeader e == col) 
                                               && evalOp op (DBE.getValue e) val) 
                                        (Set.toList $ DBT.toSet t)) 
                                (DBB.toSet ts)

product :: DBR.DBRelation -> DBR.DBRelation -> DBR.DBEvaluator DBR.DBRelation
product (DBR.DBRelation hs1 ts1) (DBR.DBRelation hs2 ts2) =
    if Set.null $ Set.intersection (DBH.toSet hs1) (DBH.toSet hs2)
        then 
            return DBR.DBRelation{
                DBR.headers = DBH.fromSet $ 
                                Set.union (DBH.toSet hs1) (DBH.toSet hs2), 
                DBR.tuples = cartesianProd ts1 ts2
            }
        else fail "Cannot perform product because of shared headers"
    where cartesianProd tls1 tls2 = DBB.fromSet $ 
            Set.fromList [
                DBT.fromSet (DBT.toSet i `Set.union` DBT.toSet j) | 
                    i <- Set.toList $ DBB.toSet tls1, 
                    j <- Set.toList $ DBB.toSet tls2
            ]

union :: DBR.DBRelation -> DBR.DBRelation -> DBR.DBEvaluator DBR.DBRelation
union (DBR.DBRelation hs1 ts1) (DBR.DBRelation hs2 ts2) =
    if hs1 == hs2
        then 
            return DBR.DBRelation{
                DBR.headers = hs1, 
                DBR.tuples = DBB.fromSet (DBB.toSet ts1 `Set.union` DBB.toSet ts2)
            }
        else fail "Cannot perfrom union on relations with mismatching headers"

intersection :: DBR.DBRelation -> DBR.DBRelation -> DBR.DBEvaluator DBR.DBRelation
intersection (DBR.DBRelation hs1 ts1) (DBR.DBRelation hs2 ts2) =
    if hs1 == hs2
        then 
            return DBR.DBRelation{
                DBR.headers=hs1, 
                DBR.tuples= DBB.fromSet $ Set.intersection (DBB.toSet ts1) (DBB.toSet ts2)
            }
        else fail "Cannot perfrom intersection on relations with mismatching headers"

difference :: DBR.DBRelation -> DBR.DBRelation -> DBR.DBEvaluator DBR.DBRelation
difference (DBR.DBRelation hs1 ts1) (DBR.DBRelation hs2 ts2) =
    if hs1 == hs2
        then 
            return DBR.DBRelation{
                DBR.headers=hs1, 
                DBR.tuples= DBB.fromSet $ Set.difference (DBB.toSet ts1) (DBB.toSet ts2)
            }
        else fail "Cannot perfrom difference on relations with mismatching headers"

join :: DBR.DBRelation -> DBR.DBRelation -> DBR.DBEvaluator DBR.DBRelation
join = undefined

data RelationalExp
  = RelationDecl DBR.DBRelation
  | Projection DBHeaders RelationalExp
  | Restriction DBPredicate RelationalExp
  | Product RelationalExp RelationalExp
  | Union RelationalExp RelationalExp
  | Intersection RelationalExp RelationalExp
  | Difference RelationalExp RelationalExp
  | Join RelationalExp RelationalExp


type RelValue = DBR.DBEvaluator DBR.DBRelation

eval :: Expr -> RelValue
eval e = ev (unExpr e) where
  ev (RelationDecl rel)           = return rel
  ev (Projection hs rel_e)        = project hs =<< ev rel_e
  ev (Restriction p rel_e)        = restrict p =<< ev rel_e
  ev (Product rel_e1 rel_e2)        = do
                                    first  <- ev rel_e1
                                    second <- ev rel_e2
                                    Relational.product first second
  ev (Union rel_e1 rel_e2)        = do
                                    first  <- ev rel_e1
                                    second <- ev rel_e2
                                    Relational.union first second
  ev (Intersection rel_e1 rel_e2) = do
                                    first  <- ev rel_e1
                                    second <- ev rel_e2
                                    Relational.intersection first second
  ev (Difference rel_e1 rel_e2)   = do
                                    first  <- ev rel_e1
                                    second <- ev rel_e2
                                    Relational.difference first second
  ev (Join rel_e1 rel_e2)         = do
                                    first  <- ev rel_e1
                                    second <- ev rel_e2
                                    Relational.join first second

newtype Expr = Expr { unExpr :: forall a . RelationalExp }
