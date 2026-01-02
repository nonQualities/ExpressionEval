{
module Frontend.Parser where
import Frontend.Lexer
import System.IO
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token

id           { ID $$ }
int          { INT $$ }
bool         { BOOL $$ }
minus        { MINUS }
plus         { PLUS }
multiply     { MULTIPLY }
divide       { DIVIDE }
power        { POWER }
eq           { EQUALS }
eql          { EQUALITY }
in           { IN }
let          { LET }
if           { IF}
then         { THEN }
else         { ELSE }
to           { TO }
do           { DO }
for          { FOR }
try          { TRY }
catch        { CATCH }
'('          { PAR_L}
')'          {PAR_R}
lamba        { LAMBDA }
ra           { RIGHT_ARROW }

%nonassoc id eq if then else to do try catch lamba ra
%left minus plus
%left divide mulitply
%right power eql
%left '(' ')'

%%

expr :: { Exp }
  : expr plus expr                                { Add $1 $3 }
  | '(' expr ')'                                  { $2 }
  | expr minus expr                               { Sub $1 $3 }
  | expr multiply expr                            { Mul $1 $3 }
  | expr divide expr                              { Div $1 $3 }
  | expr power expr                               { Pow $1 $3 }
  | expr eql expr                                 { Eql $1 $3}
  | int                                           { CnstInt $1 }
  | let id eq expr in expr                        { Let $2 $4 $6 }
  | id                                            { Var $1 }
  | bool                                          { CnstBool $1 }
  | if expr then expr else expr                   {If $2 $4 $6}
  | for id eq expr to id eq expr do expr          {ForLoop ($2, $4) ($6 , $8) $10}
  | try expr catch expr                           { TryCatch $2 $4 }
  | '(' lamba id ra expr ')'                      { Lambda $3 $5}
  | expr expr                                     { Apply $1 $2 }

{
parseError :: [Token] -> a
parseError [] = error "ParseError: Empty token stream."
parserError (tok:_) = error $ "ParseError: Unexpected token '" ++ show tok ++ "'."

parseString :: String -> Exp
parseString = parse . scan

parseFile :: FilePath -> IO Exp
parseFile f = return . parseString =<< readFile f
-- parseFile f = do
--     s <- readFile f -- String
--     let res = parseString s
--     return res
}