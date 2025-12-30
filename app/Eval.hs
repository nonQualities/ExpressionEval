module Eval (
    Val(..),
    eval,
    envEmpty
) where

import AST
import Control.Exception

data Val 
    = ValInt Int
    | ValBool Bool
    deriving (Eq, Show)

type Env = [(Vname, Val)]

envEmpty::Env
envEmpty = []

envExtend :: Vname -> Val -> Env -> Env
envExtend v val env = (v,val) : env

envLookup :: Vname -> Env -> Maybe Val
envLookup = lookup

evalIntBinOp :: (Int -> Int -> Either Error Int) -> Exp -> Exp -> Either Error Val
evalIntBinOp f eval e1 e2 =
    case (eval e1, eval e2) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right(ValInt x), Right(ValInt y)) -> case f x y of
            Left err -> Left err
            Right z -> Right $ ValInt z
        (Right _ , Right _) -> Left "Non integer operand"

evalIntBinOp' :: (Int -> Int -> Int) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f env e1 e2 =
    evalIntBinOp f' env e1 e2
    where 
        f' :: Int -> Int -> Either Error Int
        f' x y = Right $ f x y

type Error = String

eval :: Env -> Exp -> Either Error Val
eval _ env (CnstInt x) = Right $ ValInt x
eval _ env (CnstBool x) = Right $ ValBool b
eval env (Var v) = case envLookup v env of
    Just x -> Right x
    Nothing -> Left $ "Unkown variable: " ++ v
eval _ env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval _ env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval _ env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval _ env (Div e1 e2) = evalIntBinOp' checkdiv env e1 e2
    where
        checkdiv :: Int -> Int -> Either Error Val
        checkdiv _ 0 = Left "Division by zero not possible"
        checkdiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkPow env e1 e2
    where
        checkPow x y =
            if y < 0 then
                Left "Negative Exponent"
            else
                Right $ x ^ y
eval env (Eql e1 e2) =
    case(eval env e1, eval env e2) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x ==y
        (Right _, Right _) -> Left "Invalid operands to equality"
eval env (Let var e1 e2) =
    case eval env e1 of
        Left err -> Left err
        Right v -> eval (envExtend var v env) e2


