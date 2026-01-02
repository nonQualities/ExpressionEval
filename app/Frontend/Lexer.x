{
module Frontend.Lexer (Token(..), alexScanTokens, scan) where
}

%wrapper "basic"

$num = 0-9
$str = [a-zA-Z]

@sign = [\-\+]
@decimal = $num+

@int = @decimal

tokens :-

$white+                     ;
"--".*                      ;
"-"                         { \_ -> MINUS }
"+"                         { \_ -> PLUS }
"*"                         { \_ -> MULTIPLY }
"/"                         { \_ -> DIVIDE }
"^"                         { \_ -> POWER }
"="                         { \_ -> EQUALS }
"=="                        { \_ -> EQUALITY }
"("                         {\_ -> PAR_L}
")"                         {\_ -> PAR_R}
"lambda"                    {\_ -> LAMBDA}
"->"                        {\_ -> RIGHT_ARROW}
"in"                        { \_ -> IN }
"let"                       { \_ -> LET }
"true"                      { \_ -> BOOL True }
"false"                     { \_ -> BOOL False }
"if"                        {\_ -> IF}
"then"                      {\_ -> THEN}
"else"                      {\_ -> ELSE}
"to"                        {\_ -> TO}
"do"                        {\_ -> DO}
"for"                       {\_ -> FOR}
"try"                       {\_ -> TRY }
"catch"                     {\_ -> CATCH}
@int                        { \n -> INT (read n) }
$str [$str $num \']*        { \s -> ID s }

{
data Token
  = ID String
  | INT Int
  | BOOL Bool
  | DIVIDE
  | MINUS
  | PLUS
  | MULTIPLY
  | POWER
  | EQUALS
  | EQUALITY
  | IF
  | THEN
  | ELSE
  | IN
  | LET
  | FOR
  | TO
  | DO
  | TRY
  | CATCH
  | PAR_L
  | PAR_R
  | LAMBDA
  | RIGHT_ARROW
  deriving (Eq, Show)

scan :: String -> [Token]
scan = alexScanTokens
}