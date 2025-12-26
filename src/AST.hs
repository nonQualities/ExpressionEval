module AST(

) where

data Exp = Constant Int 
    | Add Exp Exp
    | Sub Exp Exp
    | Div Exp Exp
    | Mul Exp Exp 