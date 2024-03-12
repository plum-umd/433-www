module Interpreter where

import Control.Monad 

import Data.Map(Map)
import qualified Data.Map as Map

data Exp = Int Int
         | Var   String
         | Inc   String
         | Plus  Exp Exp
         | Minus Exp Exp
         | Mult  Exp Exp
         | Div   Exp Exp

type Store = Map String Int

eval :: Store -> Exp -> Either String (Int, Store)
eval s (Int n) = Right (n, s)
eval s (Var x) =
  case Map.lookup x s of
    Nothing -> Left "Uninitialized variable/Var"
    Just n -> Right (n, s)
eval s (Inc x) =
  case Map.lookup x s of
    Nothing -> Left "Uninitialized variable/Inc"
    Just n -> Right (n, Map.adjust (+1) x s)
eval s (Plus e1 e2) =
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
eval s (Mult e1 e2) =
  case eval s e1 of
    Left err -> Left err
    Right (n1,s1) ->
      case eval s1 e2 of
        Left err -> Left err
        Right (n2,s2) -> Right (n1 * n2, s2)
eval s (Div e1 e2) =
  case eval s e1 of
    Left err -> Left err
    Right (n1,s1) ->
      case eval s1 e2 of
        Left err -> Left err
        Right (0,_)  -> Left "Divide by Zero"
        Right (n2,s2) -> Right (n1 `div` n2, s2)

ex1, ex2, ex3, ex4, ex5 :: Exp
ex1 = Plus (Int 0) (Int 42)
ex2 = Mult (Int 6) (Minus (Int 8) (Int 1))
ex3 = Div ex2 ex1
ex4 = Plus (Var "x") (Int 5)
ex5 = Mult (Inc "x") (Var "x")
