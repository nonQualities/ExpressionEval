{
module Frontend.Lexer (Token(..), alexScanTokens, scan) where
}

%wrapper "basic"
$str =[a-zA-z]

$num = 0-9
@sign = [\-\+]
@decimal = $num+
@int = @decimal

tokens :-

$white+                 ;
"--".*                  ;
"-"                     {\_ -> MINUS}
"+"                     {\_ -> PLUS}
"*"                     {\_ -> MULTIPLY}
"/"                     {\_ -> DIVIDE}
"^"                     {\_ -> POWER}
"in"                    {\_ -> IN }
"="                     {\n -> INT(read n)}
"=="                    {\_ -> EQUALITY}
"let"                   {\- -> LET}
"true"                  {\_ -> BOOL TRUE}
"false"                 {\_ -> BOOL FALSE}
@int                    {\n -> INT(read n)}
$str [$str $num \ `]*   {\s -> ID s}


{
data Token 
    = ID String
    | INT Int
    | BOOL Bool
    | DIVIDE
    | MINUS
    | PLUS
    | MULTIPLY
    | EQUALS
    | EQUALITY
    | POW
    | IN 
    | LET
    | TRUE
    | FALSE
    deriving (Eq,Show)

scan :: String -> [Token]
scan = alexScanTokens

}


