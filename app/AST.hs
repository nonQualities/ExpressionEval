module AST(
    Exp(..),
    Vname
) where

type Vname = String


data Exp = CnstBool Bool
    | CnstInt Int 
    | Add Exp Exp
    | Sub Exp Exp
    | Div Exp Exp
    | Mul Exp Exp
    | Pow Exp Exp
    | Eql Exp Exp 
    | If Exp Exp Exp
    | Var Vname --var usage
    | Let Vname Exp Exp -- init and declaration and continuation
    | ForLoop (Vname, Exp) (Vname, Exp) Exp --for p = someExp in i = anotherExp do third Exp
    | Lambda Vname Exp
    | Apply Exp Exp
    | TryCatch Exp Exp 
    deriving (Eq, Show)
