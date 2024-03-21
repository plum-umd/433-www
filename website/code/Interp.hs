module Interp where

import Data.Map(Map)
import qualified Data.Map as Map

data Exp = Int Int
         | Var String
         | Inc String  -- (x++)
         | Plus  Exp Exp
         | Minus Exp Exp
         | Mult  Exp Exp
         | Div   Exp Exp
         deriving (Eq, Show)

type Store = Map String Int

eval :: Store -> Exp -> Either String (Int, Store)
eval s (Int n) = Right (n,s)
eval s (Var x) =
  case Map.lookup x s of
    Nothing -> Left "Uninitialized variable/var"
    Just n -> Right (n,s)
eval s (Inc x) =
  case Map.lookup x s of
    Nothing -> Left "Uninitialized variable/inc"
    Just n -> Right (n, Map.adjust (+1) x s) 
eval s (Plus  e1 e2) =
  case eval s e1 of
    Left err -> Left err
    Right (n1,s1) ->
      case eval s1 e2 of
        Left err -> Left err        
        Right (n2,s2) -> Right (n1 + n2, s2)
eval s (Minus e1 e2) =
  case eval s e1 of
    Left err -> Left err
    Right (n1,s1) ->
      case eval s1 e2 of
        Left err -> Left err        
        Right (n2,s2) -> Right (n1 - n2, s2)
eval s (Mult  e1 e2) =
  case eval s e1 of
    Left err -> Left err
    Right (n1,s1) ->
      case eval s1 e2 of
        Left err -> Left err        
        Right (n2,s2) -> Right (n1 * n2, s2)
eval s (Div   e1 e2) =
  case eval s e1 of
    Left err -> Left err
    Right (n1,s1) ->
      case eval s1 e2 of
        Left err -> Left err
        Right (0,_)  -> Left "Division by zero"
        Right (n2,s2) -> Right (n1 `div` n2, s2)

ex1 = Plus (Int 40) (Int 2)
ex2 = Mult (Int 6) (Minus (Int 10) (Int 1))
ex3 = (Div (Mult (Int 2) ex1) ex1)
ex4 = Div (Int 1) (Int 0)
ex5 = Plus (Var "x") (Int 8)
ex6 = Mult (Inc "x") (Var "x")

m0 = Map.empty
m1 = Map.singleton "x" 6
