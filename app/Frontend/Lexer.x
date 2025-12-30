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
"in"                        { \_ -> IN }
"let"                       { \_ -> LET }
"true"                      { \_ -> BOOL True }
"false"                     { \_ -> BOOL False }
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
  | IN
  | LET
  deriving (Eq, Show)

scan :: String -> [Token]
scan = alexScanTokens
}