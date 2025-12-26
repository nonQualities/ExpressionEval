{
module Frontend.Lexer (Token(..), alexScanTokens, scan) where
}

%wrapper "basic"

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
@int                    {\n -> INT(read n)}

{
data Token 
    = INT int
    | DIVIDE
    | MINUS
    | PLUS
    | MULTIPLY
    deriving (Eq,Show)

scan :: String -> [Token]
scan = alexScanTokens

}


