{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module DBRelation(DBRelation(..), DBEvaluator) where

import Data.List (intercalate)
import Text.Printf()

import DBHeaders(DBHeaders)
import DBRelBody(DBRelBody)


newtype DBEvaluator a = DBEvaluatorImpl (Either String a) deriving (Show, Applicative, Functor)
instance Monad DBEvaluator where
    (DBEvaluatorImpl ev) >>= k =
        case ev of
          Left msg -> DBEvaluatorImpl (Left msg)
          Right v -> k v
    return v = DBEvaluatorImpl (Right v)
    fail msg = DBEvaluatorImpl (Left msg)

data DBRelation = DBRelation {headers :: DBHeaders, tuples :: DBRelBody}

instance Show DBRelation where
    show (DBRelation hs ts) = intercalate "\n" [show hs, show ts]
