module Eval(
    Val(..),
    eval
) where

import AST
import Control.Exception

data Val 
    = ValInt Int
    deriving (Eq, Show)

eval :: Exp -> Val
eval (Constant x) = ValInt x
eval ( Add e1 e2) = 
   case(eval e1, eval e2) of
       (ValInt v1, ValInt v2) -> ValInt $ v1 + v2
eval (Sub e1 e2) = 
   case(eval e1, eval e2) of
       (ValInt v1, ValInt v2) -> ValInt $ v1 - v2
eval(Mul e1 e2) = 
    case(eval e1, eval e2) of
       (ValInt v1, ValInt v2) -> ValInt $ v1 * v2
eval(Div e1 e2) = 
    case(eval e1, eval e2) of
       (ValInt v1, ValInt v2) -> 
        if v2 == 0 then
            error "Division by zero"
        else ValInt $ v1 `div` v2

