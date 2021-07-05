module Transformers where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe

-- Types

type Name = String

data Exp
  = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value
  = IntVal Integer
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value

-- Eval0

eval0 :: Env -> Exp -> Value
eval0 env (Lit n) = IntVal n
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
  let IntVal n1 = eval0 env e1
      IntVal n2 = eval0 env e2
   in IntVal (n1 + n2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let v1 = eval0 env e1
      v2 = eval0 env e2
   in case v1 of
        FunVal env' n body -> eval0 (Map.insert n v2 env') body

exampleExp :: Exp
exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

-- >>> eval0 Map.empty exampleExp
-- IntVal 18

-- Eval1

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

-- eval1 :: Env -> Exp -> Eval1 Value
eval1 :: Monad m => Env -> Exp -> m Value
eval1 env (Lit n) = pure (IntVal n)
eval1 env (Var n) = maybe (error ("undefined variable: " ++ n)) pure (Map.lookup n env)
eval1 env (Plus e1 e2) = do
  v1 <- eval1 env e1
  v2 <- eval1 env e2
  case (v1, v2) of
    (IntVal n1, IntVal n2) -> pure (IntVal (n1 + n2))
eval1 env (Abs n e) = pure (FunVal env n e)
eval1 env (App e1 e2) = do
  v1 <- eval1 env e1
  v2 <- eval1 env e2
  case v1 of
    FunVal env' n body -> eval1 (Map.insert n v2 env') body

-- >>> runEval1 (eval1 Map.empty exampleExp)
-- IntVal 18

-- Eval2

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runExceptT

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit n) = pure (IntVal n)
eval2 env (Var n) = case Map.lookup n env of
  Just v -> pure v
  Nothing -> throwError ("unbound variable: " ++ n)
eval2 env (Plus e1 e2) = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case (v1, v2) of
    (IntVal n1, IntVal n2) -> pure (IntVal (n1 + n2))
    _ -> throwError "type error in addition"
eval2 env (Abs n e) = pure (FunVal env n e)
eval2 env (App e1 e2) = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case v1 of
    FunVal env' n body -> eval1 (Map.insert n v2 env') body
    _ -> throwError "type error in application"

-- >>> runEval2 (eval2 Map.empty exampleExp)
-- Right (IntVal 18)

-- >>> runEval2 (eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))
-- Left "type error in addition"

-- >>> runEval2 (eval2 Map.empty (Var "x"))
-- Left "unbound variable: x"
