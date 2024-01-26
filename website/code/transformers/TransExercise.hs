{-
---
fulltitle: "In class exercise: TransExercise"
---
-}
{-# LANGUAGE FlexibleContexts #-}

module TransExercise where

{-
This exercise involves using monad transformers to extend a simple imperative
 programming language, called `L`, with support for *exceptions*. This
 language is a minimal subset of the `Lu` programming language from an upcoming
 homework assignment, with the addition of `throw` and `try` statements.

* For simplicity, we define the syntax of this extended language in a [separate
file](LSyntax.hs).

* The test case [try.l](try.l) demonstrates the syntax for exceptions in
  the extended language.

This exercise will give you practice with the `MonadState` and `MonadError`
type classes and the `StateT` and `ExceptT` monad transformers that were
introduced in the [Transformers](Transformers.html) lecture. These definitions
come from the [mtl](http://hackage.haskell.org/package/mtl) library.

-}

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), State, StateT, runState, runStateT)
import Control.Monad.Trans(lift)
import Data.Map (Map)
import qualified Data.Map as Map
import LSyntax
  ( Block (..),
    Bop (..),
    Expression (..),
    Statement (..),
    Value (..),
    Var,
  )
import Test.HUnit
  ( Test (TestCase, TestList),
    assertFailure,
    runTestTT,
    (~:),
    (~?=),
  )

{-
Expression Evaluator
--------------------

1. First, make sure that you understand how the expression evaluator works.
   Compared to the language of your homework, `L` lacks tables and most of
   the operators. Instead, it only includes global variables, arithmetic operators,
   ints, and `nil`.  As a result, the store in this language can just be a flat
   map from variables to values.
-}

type Store = Map Var Value

-- remove this type annotation to reveal a more general type for this function
-- evalE :: Expression -> State Store Value
evalE :: (MonadState (Map Var Value) m, MonadError Value m) =>
         Expression -> m Value
evalE (Var x) = do
  m <- get
  case Map.lookup x m of
    Just v -> return v
    Nothing -> throwError (IntVal 0)
evalE (Val v) = return v
evalE (Op2 e1 o e2) = do
  v1 <- evalE e1
  v2 <- evalE e2
  evalOp2 o v1 v2

evalOp2 Plus   (IntVal i1) (IntVal i2) = return $ IntVal (i1 + i2)
evalOp2 Minus  (IntVal i1) (IntVal i2) = return $ IntVal (i1 - i2)
evalOp2 Times  (IntVal i1) (IntVal i2) = return $ IntVal (i1 * i2)
evalOp2 Divide (IntVal _ ) (IntVal 0 ) = throwError (IntVal 1)
evalOp2 Divide (IntVal i1) (IntVal i2) = return $ IntVal (i1 `div` i2)
evalOp2 _ _ _ = throwError (IntVal 2)

{-
2. Next, modify `evalOp2` and `evalE` above so that they use `throwError` (from the `MonadError` class) for runtime errors

   - use code `IntVal 0` for undefined variables
   - use code `IntVal 1` in the case of divide by zero
   - use code `IntVal 2` in the case of invalid args to a numeric operator

  The types of these functions will be different once you make this modification.

Running and Testing the expression evaluator
------------------------------

To test the expression evaluator we have to pick a specific monad to use; one
that satisfies both `MonadState` and `MonadError` constraints.

We can construct this monad easily by layering the exception monad on top of
the usual `State` monad.
-}

type M = ExceptT Value (State Store)

{-
Now we can run expressions that may throw errors!
-}

executeE :: Expression -> Store -> (Either Value Value, Store)
executeE e = runState (runExceptT comp)
  where
    comp :: M Value
    comp = evalE e

{-
We can display the errors nicely for experimentation in ghci with
this function. (The `display` function is defined at the end of the file).
-}

runE :: Expression -> IO ()
runE e = putStrLn $ display (fst (executeE e Map.empty))

{-
For example, try these out:
-}

-- "1 / 0"
-- >>> display (fst (executeE (Op2  (Val (IntVal 1)) Divide (Val (IntVal 0))) Map.empty))

-- "1 / 1"
-- >>> display (fst (executeE (Op2  (Val (IntVal 1)) Divide (Val (IntVal 1))) Map.empty))

{-
We can also write tests that expect a particular execution to
raise a particular error.
-}

raisesE :: Expression -> Value -> Test
s `raisesE` v = case executeE s Map.empty of
  (Left v', _) -> v ~?= v'
  _ -> TestCase $ assertFailure "Error in raises"

{-
Make sure that your implementation above passes these tests.
-}

test_undefined :: Test
test_undefined =
  "undefined variable"
    ~: (Var "Y" `raisesE` IntVal 0)

divByZero :: Expression
divByZero = Op2 (Val (IntVal 1)) Divide (Val (IntVal 0))

test_divByZero :: Test
test_divByZero = "divide by zero" ~: divByZero `raisesE` IntVal 1

badPlus :: Expression
badPlus = Op2 (Val (IntVal 1)) Plus (Val NilVal)

test_badPlus :: Test
test_badPlus = "bad arg to plus" ~: badPlus `raisesE` IntVal 2

test_expErrors :: Test
test_expErrors =
  "undefined variable & division by zero"
    ~: TestList [test_undefined, test_divByZero, test_badPlus]

-- >>> runTestTT test_expErrors

{-
Statement Evaluator
-------------------

3. There is not much to do in this step except notice
  that changing the type of `evalE` changes the type of `evalS`.  You don't
  need to implemnt `Try` and `Throw` just yet.
-}

evalCondition :: Value -> Bool
evalCondition (IntVal 0) = False -- since we don't have bools, use 0 & nil as false
evalCondition NilVal = False
evalCondition _ = True

-- type annotation intentionally not given
--evalS :: Statement -> M ()
evalS (If e s1 s2) = do
  v <- evalE e
  if evalCondition v then eval s1 else eval s2
evalS (Assign x e) = do
  v <- evalE e
  m <- get
  put (Map.insert x v m)
evalS w@(While e ss) = do
  v <- evalE e
  when (evalCondition v) $ do
    eval ss
    evalS w
evalS (Try ss1 x ss2) =
  eval ss1 `catchError` \v -> do
    m <- get
    put (Map.insert x v m)
    eval ss2
-- Way 1:
--   do
--   m <- get 
--   let (res, m') = runState (runExceptT (eval ss1)) m
--   case res of
--     Left v -> do
--       put (Map.insert x v m')
--       eval ss2
--     Right () -> return ()
-- Way 2:
--   e <- lift (runExceptT (eval ss1))
--   m <- get
--   case e of
--     Left v -> do
--       put (Map.insert x v m)
--       eval ss2
--     Right () -> return ()
evalS (Throw e) = do
  v <- evalE e
  throwError v

-- type annotation intentionally not given
--eval :: Block -> M ()
eval (Block ss) = mapM_ evalS ss

{-
4. Now finish this function for Statement execution. (Check out
  `executeE` for a hint.)
-}

execute :: Block -> Store -> (Either Value (), Store)
execute b st =
  runState (runExceptT (eval b)) st
  

{-
Try out your `execute` with this operation:
-}

run :: Block -> String
run block =
  let (r, s) = execute block Map.empty
   in display r ++ ",  Store: " ++ show s

{-
For example:
-}

-- >>> run $ Block [While badPlus (Block [])]

{-
Test your functions with this helper
-}

raises :: Block -> Value -> Test
s `raises` v = case execute s Map.empty of
  (Left v', _) -> v ~?= v'
  _ -> TestCase $ assertFailure "Error in raises"

{-
and these tests:
-}

test_badWhile :: Test
test_badWhile = Block [While (Var "Y") (Block [])] `raises` IntVal 0

test_badIf :: Test
test_badIf = Block [If divByZero (Block []) (Block [])] `raises` IntVal 1

-- >>> runTestTT test_badWhile

-- >>> runTestTT test_badIf

{-
5. Add user-level exceptions

There are two new statement forms in this language. Extend the evaluator above
so that it can handle them.

- `Throw e` should evaluate the expression `e` and trigger an exception
  carrying the value of `e`

- `Try s x h` should execute the statement `s` and if, in the course
  of execution, an exception is thrown, then the exception value should
  be assigned to the variable `x` after which the *handler* statement
  `h` should be executed.

Note: the `catchError` function in
[Control.Monad.Except](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html)
will be necessary for `Try` statements.

For example, this block is the abstract syntax for [try.l](try.l).
-}

tryExpr :: Block
tryExpr = Block [Assign "x" (Val (IntVal 0)), Assign "y" (Val (IntVal 1)), Try (Block [If (Op2 (Var "x") Plus (Var "y")) (Block [Assign "a" (Val (IntVal 100)), Throw (Op2 (Var "x") Plus (Var "y")), Assign "b" (Val (IntVal 200))]) (Block [])]) "e" (Block [Assign "z" (Op2 (Var "e") Plus (Var "a"))])]

-- >>> run tryExpr

{-
Should print

       Result: ()
       Output Store: fromList [("a",IntVal 100),("e",IntVal 1),("x",IntVal 0),("y",IntVal 1),("z",IntVal 101)]

Displaying the results
-----------------------

-}

-- display the result of evaluation
display :: Show a => Either Value a -> String
display (Left v) = "Uncaught exception: " ++ displayExn v
display (Right v) = "Result: " ++ show v

-- decode an exception value
displayExn :: Value -> String
displayExn (IntVal 0) = "Undefined variable"
displayExn (IntVal 1) = "Divide by zero"
displayExn (IntVal 2) = "Invalid arguments to operator"
displayExn v = "Unknown error code: " ++ show v
