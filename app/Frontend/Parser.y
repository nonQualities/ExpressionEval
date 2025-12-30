{
module Frontend.Parser where
import Frontend.Lexer
import System.IO
import AST
}

%name Parser
%tokentype {Token}
%error { parseError }

%token
id           {ID $$ }
int          {INT $$}
bool         {BOOL $$}
minus        {MINUS}
plus         {PLUS}
multiply     {MULTIPLY}
divide       {DIVIDE}
power        {POWER}
eql          {EQUALITY}
eq           {EQUALS}
in           {IN}
let          {LET}



%nonassoc id eq 
%left minus plus
%left divide multiply
%right power eql


%%

--production
expr :: { Exp }
    : expr plus expr            {Add $1 $3}
    | expr minus expr           {Sub $1 $3}
    | expr multiply expr        {Multiply $1 $3}
    | expr divide expr          {Divide $1 $3}
    | expr power expr           {Power $1 $3}
    | expr eql expr             {Eql $1 $3}
    | int                       {CstInt $1}
    | let id eq expr in expr    {Let $2 $4 $6}
    | id                        {Var $1}
    | bool                      {CstBool $1}


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

