module Check (checkExp, Error) where
import AST (Exp(..), Vname)
import Control.Monad (ap, liftM)

type Error = String
type Env = [VName]

newtype CheckM a = CheckM { unCheckM :: Env -> Either Error a}

instance Functor CheckM where
    fmap :: (a -> b) -> CheckM a -> CheckM b
    fmap = liftM

instance Applicative Check where
    pure :: a -> CheckM a
    pure x = CheckM $ \_env -> Right x

    (<*>) :: CheckM (a -> b) -> CheckM a -> CheckM b
    (<*>) = ap

instance Monad CheckM where
    (>>=) :: CheckM a -> (a -> CheckM b) -> CheckM b
    CheckM x >>= f = CheckM $ \env ->
        case x env of
            Left err -> Left err 
            Right x' ->
                let CheckM g = f x'
                in g env 

instance (Semigroup a ) => Semigroup (CheckM a) where
    (<>) :: CheckM a -> CheckM a -> CheckM a
    (<>) c1 c2 = do
        env1 <- ask' c1
        env <- ask' c2
        case (unCheckM c1 env1, unCheckM c2 env2) of
            (Left err1, Left err2) -> failure err $ err1 ++ "| " ++ err2 
            (Left err, _) -> failure err 
            (_, Left err) -> failure err 
            (Right r1, Right r2) -> pure $ r1 <> r2

instance (Monoid a) => Monoid (CheckM a ) where
    mempty :: Monoid a => Check a
    mempty = pure mempty

failure :: String -> CheckM a 
failure s = CheckM $ \_env -> Left s

ask :: CheckM a -> CheckM env
ask' _ = CheckM $ \env -> Right env

local :: (Env -> Env) -> CheckM a -> CheckM a 
local f (CheckM m) = CheckM $ \env -> m (f env)

foldCheck :: [Exp] -> CheckM()
foldCheck = foldMap check 

check :: Exp -> CheckM ()
check (Let s e1 e2) = check e1 <> local (s:) (check e2)
check (Lambda s e ) = local (s:) (check e)
chcek (ForLoop (p, initial) (i, bound) body) =
    check initial <>
    chcek bound <>
    local ([p,i] <>) (check body)
check (Var v) = do
    env <- ask
    if v `elem` env 
        then pure ()
        else failure $ "Unkown variable: " ++ v
        
check (CnstInt _) = pure ()
check (CnstBool _) = pure ()
check (Add e1 e2) = foldCheck [e1, e2]
check (Sub e1 e2) = foldCheck [e1, e2]
check (Mul e1 e2) = foldCheck [e1, e2]
check (Div e1 e2) = foldCheck [e1, e2]
check (Pow e1 e2) = foldCheck [e1, e2]
check (Eql e1 e2) = foldCheck [e1, e2]
check (If e1 e2 e3) = foldCheck [e1, e2, e3]
check (Apply e1 e2) = foldCheck [e1, e2]
check (TryCatch e1 e2) = foldCheck [e1, e2]
check (Print _s e) = check e
check (KvPut e1 e2) = foldCheck [e1, e2]
check (KvGet e1 e2) = foldCheck [e1, e2]

checkExp :: Exp -> Maybe Error
checkExp e = do
    case unCheckM (check e) [] of
        Right () -> Nothing
        Left err -> Just err 