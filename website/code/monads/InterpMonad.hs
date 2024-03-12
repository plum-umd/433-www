module InterpMonad where

import Control.Monad 
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as Map

data Exp = Int Int
         | Var   String
         | Inc   String
         | Plus  Exp Exp
         | Minus Exp Exp
         | Mult  Exp Exp
         | Div   Exp Exp

eval :: Exp -> M Int
eval (Int n) = return n
eval (Var x) = look x
eval (Inc x) = do
  n <- look x
  inc x
  return n
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
  guard' "Divide by zero" (n2 /= 0)
  return $ n1 `div` n2

guard' :: String -> Bool -> M ()
guard' _ True = return ()
guard' s False = lift $ Left s

inc :: String -> M Int
inc x = do
  s <- get
  put (Map.adjust (+1) x s)
  lift $ case Map.lookup x s of
           Nothing -> Left "Unitialized Variable"
           Just n -> Right n

look :: String -> M Int
look x = do
  s <- get
  lift $ case Map.lookup x s of
           Nothing -> Left "Unitialized Variable"
           Just n -> Right n
  
type Store = Map String Int
type M = StateT Store (Either String)

{-
newtype M a = M {run :: Store -> Either String (a, Store)}

instance Alternative M where
  empty = M $ \s -> Left ""

  m1 <|> m2 = M $ \s ->
    case run m1 s of
      Just (x,s')  -> Just (x, s')
      Nothing -> run m2 s

instance Monad M where
  m >>= k = M $ \s ->
    case run m s of
      Just (a,s') -> run (k a) s'
      Nothing -> Nothing 

instance Applicative M where
  pure a = M $ \s -> Just (a,s)
  (<*>) = ap

instance Functor M where
  fmap = liftM
-}

ex1 = Plus (Int 0) (Int 42)
ex2 = Mult (Int 6) (Minus (Int 8) (Int 1))
ex3 = Div ex2 ex1
ex4 = Plus (Var "x") (Int 5)
ex5 = Mult (Inc "x") (Var "x")
