module Eval (
    Val(..),
    eval,
    envEmpty
) where

import AST

data Val
    = ValInt Int
    | ValBool Bool
    deriving (Eq, Show)

type Env = [(Vname, Val)]

type Error = String

envEmpty :: Env
envEmpty = []

envExtend :: Vname -> Val -> Env -> Env
envExtend v val env = (v,val) : env

envLookup :: Vname -> Env -> Maybe Val
envLookup = lookup

-- 1. Helper for operations that MIGHT fail (like Div)
-- Returns Either Error Int, which we then wrap in ValInt
evalIntBinOp :: (Int -> Int -> Either Error Int) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp f env e1 e2 =
    case (eval env e1, eval env e2) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (ValInt x), Right (ValInt y)) ->
            case f x y of
                Left err -> Left err
                Right z  -> Right $ ValInt z
        (Right _, Right _) -> Left "Type Error: Non-integer operand in integer operation"

-- 2. Helper for operations that CANNOT fail (like Add, Mul)
-- Wraps the simple Int -> Int -> Int function into the error-handling type
evalIntBinOp' :: (Int -> Int -> Int) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f = evalIntBinOp (\x y -> Right (f x y))

eval :: Env -> Exp -> Either Error Val
-- Fixed: Removed the extra '_' argument to match type signature
eval _ (CnstInt x) = Right $ ValInt x
eval _ (CnstBool x) = Right $ ValBool x
eval env (Var v) = case envLookup v env of
    Just x  -> Right x
    Nothing -> Left $ "Unknown variable: " ++ v

-- Binary Operations
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2

-- Div needs the error-checking helper (evalIntBinOp), not the safe one
eval env (Div e1 e2) = evalIntBinOp safeDiv env e1 e2
  where
    safeDiv :: Int -> Int -> Either Error Int
    safeDiv _ 0 = Left "Division by zero"
    safeDiv x y = Right (x `div` y)

eval env (Pow e1 e2) = evalIntBinOp safePow env e1 e2
    where
        safePow :: Int -> Int -> Either Error Int
        safePow x y = if y < 0 then Left "Negative Exponent" else Right (x ^ y)

eval env (Eql e1 e2) =
    case (eval env e1, eval env e2) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool (x == y)
        (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool (x == y) -- Added Bool equality for completeness
        (Right _, Right _) -> Left "Type Error: Invalid operands to equality"

eval env (Let var e1 e2) =
    case eval env e1 of
        Left err -> Left err
        Right v  -> eval (envExtend var v env) e2