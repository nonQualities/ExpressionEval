module AST(
    Exp(..),
    Vname
) where

type vname = String


data Exp = CnstBool Bool
    | CnstInt Int 
    | Add Exp Exp
    | Sub Exp Exp
    | Div Exp Exp
    | Mul Exp Exp
    | Pow Exp Exp
    | Eql Exp Exp 
    | Var Vname --var usage
    | Let VName Exp Exp -- init and declaration
