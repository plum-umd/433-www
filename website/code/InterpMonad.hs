module Mon where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as Map

data Exp = Int Int
         | Var String
         | Inc String
         | Plus  Exp Exp
         | Minus Exp Exp
         | Mult  Exp Exp
         | Div   Exp Exp 
         deriving (Eq, Show)

type Store = Map String Int
type M = StateT Store Maybe

-- get :: State s s
-- put :: s -> State s () 

eval :: Exp -> M Int
eval (Int n) = return n
eval (Var x) = do 
  s <- get
  lift $ Map.lookup x s
eval (Inc x) = do 
  s <- get
  put (Map.adjust (+1) x s)
  lift $ Map.lookup x s
eval (Plus e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  return $ n1 + n2
eval (Minus e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  return $ n1 - n2
eval (Mult e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  return $ n1 * n2
eval (Div e1 e2) = do
  n1 <- eval e1
  n2 <- eval e2
  guard (n2 /= 0)
-- guard' "Division by Zero" (n2 /= 0)
  return $ n1 `div` n2

guard' :: String -> Bool -> Either String ()
guard' s True  = Right ()
guard' s False = Left s

ex1 = Plus (Int 40) (Int 2)
ex2 = Mult (Int 6) (Minus (Int 10) (Int 1))
ex3 = (Div (Mult (Int 2) ex1) ex1)
ex4 = Div (Int 1) (Int 0)
ex5 = Plus (Var "x") (Int 8)
--ex6 = Mult (Inc "x") (Var "x")

m0 = Map.empty
m1 = Map.singleton "x" 6
