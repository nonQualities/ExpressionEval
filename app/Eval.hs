{-# LANGUAGE InstanceSigs #-}
module Eval (
    Val(..),
    eval,
    envEmpty,
    runEval,
    Error
) where

import AST
import Control.Monad (ap)


-- Values and environments


data Val
    = ValInt Int
    | ValBool Bool
    | ValFun Env Vname Exp
    deriving (Eq, Show)

type Env = [(Vname, Val)]
type KvStore = [(Val, Val)]
type State = ([String], KvStore)
type Error = String


-- Evaluation monad
newtype EvalM a = EvalM (Env -> State -> (Either Error a, State))


-- Functor / Applicative / Monad (see document EvalDoc for more details)
instance Functor EvalM where
    fmap :: (a -> b) -> EvalM a -> EvalM b
    fmap f (EvalM g) = EvalM $ \env s ->
        case g env s of
            (Left err, s') -> (Left err, s')
            (Right x, s')  -> (Right (f x), s')

instance Applicative EvalM where
    pure :: a -> EvalM a
    pure x = EvalM $ \_env s -> (Right x, s)

    (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b
    (<*>) = ap

instance Monad EvalM where
    (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
    EvalM m >>= f = EvalM $ \env s ->
        case m env s of
            (Left err, s') -> (Left err, s')
            (Right x, s') ->
                let EvalM m' = f x
                in m' env s'


-- Environment helpers
askEnv :: EvalM Env
askEnv = EvalM $ \env s -> (Right env, s)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env s -> m (f env) s

envEmpty :: Env
envEmpty = []

envExtend :: Vname -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: Vname -> Env -> Maybe Val
envLookup = lookup


-- Errors and control
failure :: String -> EvalM a
failure msg = EvalM $ \_env s -> (Left msg, s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env s ->
    case m1 env s of
        (Left _, s')  -> m2 env s'
        ok            -> ok


-- Running the evaluator
stateEmpty :: State
stateEmpty = ([], [])

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
    let (res, (out, _)) = m envEmpty stateEmpty
    in (reverse out, res)


-- Integer operation helpers
evalIntBinOp :: (Int -> Int -> EvalM Int) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (ValInt x, ValInt y) -> ValInt <$> f x y
        _ -> failure "Non-integer operand"

evalIntBinOp' :: (Int -> Int -> Int) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f = evalIntBinOp (\x y -> pure (f x y))

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_env (printed, kvstore) -> (Right (), (s : printed, kvstore))

evalkvGet :: Val -> EvalM Val
evalkvGet k = do
    kvStore <- getKvStore
    case lookup k kvStore of
        Just v -> return v
        Nothing -> failure $ "Invalid key: " ++ show k


evalkvPut :: Val -> Val -> EvalM()
evalkvPut k v = do
    kvStore <- getKvStore
    putKvStore $ (k,v) : filter (\(k',_) -> k' /= k) kvStore

getKvStore :: EvalM KvStore
getKvStore = EvalM $ \_env (printed, kvStore) -> (Right kvStore, (printed, kvStore))

putKvStore :: KvStore -> EvalM()
putKvStore kvStore = EvalM $ \_env (printed, _) -> (Right (), (printed, kvStore))

-- Evaluation
eval :: Exp -> EvalM Val

-- Constants
eval (CnstInt x)  = pure (ValInt x)
eval (CnstBool x) = pure (ValBool x)

-- Variables
eval (Var v) = do
    env <- askEnv
    case envLookup v env of
        Just x  -> pure x
        Nothing -> failure ("Unknown variable: " ++ v)

-- Arithmetic
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2

eval (Div e1 e2) = evalIntBinOp safeDiv e1 e2
  where
    safeDiv _ 0 = failure "Division by zero"
    safeDiv x y = pure (x `div` y)

eval (Pow e1 e2) = evalIntBinOp safePow e1 e2
  where
    safePow _ y | y < 0 = failure "Negative exponent"
    safePow x y        = pure (x ^ y)

-- Equality
eval (Eql e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (ValInt x, ValInt y)   -> pure (ValBool (x == y))
        (ValBool x, ValBool y)-> pure (ValBool (x == y))
        _ -> failure "Invalid operands to equality"

-- Conditionals
eval (If cond e1 e2) = do
    v <- eval cond
    case v of
        ValBool True  -> eval e1
        ValBool False -> eval e2
        _ -> failure "Non-boolean condition"

-- Let binding
eval (Let v e1 e2) = do
    val <- eval e1
    localEnv (envExtend v val) (eval e2)

-- For loop (fold-style)
eval (ForLoop (p, initial) (iv, bound) body) = do
    initVal <- eval initial
    boundVal <- eval bound
    case boundVal of
        ValInt n -> loop 0 n initVal
        _ -> failure "Non-integral loop bound"
  where
    loop i n acc
        | i >= n = pure acc
        | otherwise = do
            acc' <- localEnv
                        ( envExtend iv (ValInt i)
                        . envExtend p acc
                        )
                        (eval body)
            loop (i + 1) n acc'

-- Lambda
eval (Lambda v body) = do
    env <- askEnv
    pure (ValFun env v body)

-- Application
eval (Apply e1 e2) = do
    f <- eval e1
    arg <- eval e2
    case f of
        ValFun fEnv v body ->
            localEnv (const (envExtend v arg fEnv)) (eval body)
        _ -> failure "Attempted to apply non-function"

-- Try/Catch
eval (TryCatch e1 e2) =
    eval e1 `catch` eval e2

eval (Print s e) = do
    v <- eval e
    eval Print $ s ++ ": " ++
        case v of
            ValInt i -> show i
            ValBool b -> show b
            _ -> "#<fun>"

eval (KvPut e1 e2 ) = do
    k <- eval e1
    v <- eval e2
    evalkvPut k v 
    return v

eval(KvGet e) = do
    v <- eval e
    evalkvGet v

