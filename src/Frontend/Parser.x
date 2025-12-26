{
module Frontend.Parse where
import Frontend.Lexer
import System.IO
import AST
}

%name parse
%tokentype {Token}
%error { parseError }

%token

int          {INT $$}
minus        {MINUS}
plus         {PLUS}
multiply     {MULTIPLY}
divide       {DIVIDE}

%left minus plus
%left divide multiply

%%

expr :: { Exp }
    : expr plus expr     {Add $1 $3}
    | expr minus expr    {Sub $1 $3}
    | expr multiply expr {Multiply $1 $3}
    | expr divide expr   {Divide $1 $3}
    | constant           {Constant $1}

{
parseError :: [Token] -> a
parseError [] = error "ParseError: Empty token stream"
parseError (tok:_) error $ "ParseError: Unexpected token  "++show tok ++ "."

parseString :: String -> Exp
parseString = parse . scan

parseFile :: FilePath -> IO Exp
parseFile f = return . parseString
parseFile f = return . parseString =<< readFile f
}

