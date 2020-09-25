{
module Parser.Lexer ( Token (..)
                    , lexProg ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$symb = [\_]

tokens:-
    $white+                             ;

    "if"                                { const TIf }
    "then"                              { const TThen }
    "else"                              { const TElse }
    "end"                               { const TEnd }
    "while"                             { const TWhile }
    "inv"                               { const TInv }
    "do"                                { const TDo }
    "program"                           { const TProgram }
    "is"                                { const TIs }
    "pre"                              { const TPre}
    "post"                              { const TPost}
    "forall"                            { const TAll}
    "exists"                            { const TExt}

    $digit+                             { TokenInt . read }
    $alpha [$alpha $digit $symb ]*      { TokenName }
    [\[ \] \+ \- \* \/ \% ]             { TokenSymb }
    [\( \) ]                            { TokenSymb }
    
    [\= \< \>]                          { TokenSymb }
    \!\=                                { TokenSymb }
    \>\=                                { TokenSymb }
    \<\=                                { TokenSymb }
    \!                                  { TokenSymb }
    
    \|\|                                { TokenSymb }
    \&\&                                { TokenSymb }

    \:\=                                { TokenSymb }
    \,                                  { TokenSymb }
    \;                                  { TokenSymb }
    \=\=\>                              { TokenSymb }


{
data Token = TokenInt Int
           | TokenName String
           | TokenSymb String
           | TIf
           | TThen
           | TElse
           | TEnd
           | TWhile
           | TInv
           | TDo
           | TProgram
           | TIs
           | TAll
           | TExt
           | TPre
           | TPost
           deriving (Eq, Show)

lexProg :: String -> [Token]
lexProg = alexScanTokens

}