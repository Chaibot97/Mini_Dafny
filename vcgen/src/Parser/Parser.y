{
module Parser.Parser (parseProg) where

import Language
import Parser.Lexer
}

%name parse1
%tokentype { Token }
%error { parseError }

%token
    int         { TokenInt $$ }
    name        { TokenName $$ }
    '['         { TokenSymb "[" }
    ']'         { TokenSymb "]" }
    '+'         { TokenSymb "+" }
    '-'         { TokenSymb "-" }
    '*'         { TokenSymb "*" }
    '/'         { TokenSymb "/" }
    '%'         { TokenSymb "%" }
    '('         { TokenSymb "(" }
    ')'         { TokenSymb ")" }
    
    '='         { TokenSymb "=" }
    "!="        { TokenSymb "!=" }
    "<="        { TokenSymb "<=" }
    ">="        { TokenSymb ">=" }
    '<'         { TokenSymb "<" }
    '>'         { TokenSymb ">" }
    '!'         { TokenSymb "!" }
    
    "||"        { TokenSymb "||" }
    "&&"        { TokenSymb "&&" }

    ":="        { TokenSymb ":=" }
    ','         { TokenSymb "," }
    ';'         { TokenSymb ";" }
    "if"        { TIf }
    "then"      { TThen }
    "else"      { TElse }
    "end"       { TEnd }
    "while"     { TWhile }
    "do"        { TDo }
    "inv"       { TInv }
    "program"   { TProgram }
    "is"        { TIs }

    "pre"      {TPre}
    "post"      {TPost}
    "==>"       {TokenSymb "==>"}
    "forall"   {TAll}
    "exists"   {TExt}


%left '+' '-'
%left '*' '/' '%'

%left "||"
%left "&&"
%left '!'

%%

{- parameterized productions
   taken from https://www.haskell.org/happy/doc/html/sec-grammar.html -}

rev_list_plus(p)
     : p                    { [$1] }
     | rev_list_plus(p) p   { $2 : $1 }

{- non-empty lists of components, like "p+" -}
list_plus(p)
     : rev_list_plus(p)   { reverse $1 }

{- lists, like "p*" -}
list(p)
     : {- empty -}    { [] }
     | list_plus(p)   { $1 }

prog :: { Program }
     : "program" name list(pre) list(post) "is" block "end"
       { Program{name = $2, pre = $3, post = $4, block = $6} }

pre  :: { Assertion }
     : "pre"  assertion   { $2 }

post :: { Assertion }
     : "post" assertion   { $2 }

inv  :: { Assertion }
     : "inv"  assertion   { $2 }

block :: { Block }
      : list_plus(stmt)   { $1 }

binop :: { Operator }
     : '+' { Add }
     | '-' { Sub }
     | '*' { Mul }
     | '/' { Div }
     | '%' { Mod }
 
arithExp :: { ArithExp }
         : int { Num $1 }
         | name { Var $1 }
         | '-' arithExp { BinOp Sub (Num 0) $2 }
         | name '[' arithExp ']' { Read $1 $3 }
         | arithExp binop arithExp { BinOp $2 $1 $3 }
         | '(' arithExp ')'      { $2 }
{-         | '(' arithExp ')'      { Parens $2 } -}

order :: { Order }
     : '='  { Eq }
     | "!=" { Neq }
     | "<=" { Le }
     | ">=" { Ge }
     | '<'  { Lt }
     | '>'  { Gt }

comp :: { Comparison }
     : arithExp order arithExp { Comp $2 $1 $3 }

boolExp :: { BoolExp }
        : comp { BCmp $1 }
        | '!' boolExp { BNot $2 }
        | boolExp "||" boolExp { BDisj $1 $3 }
        | boolExp "&&" boolExp { BConj $1 $3 }
        | '(' boolExp ')' { $2 }
        {- | '(' boolExp ')' { BParens $2 } -}

assertion :: { Assertion }
          : comp { ACmp $1 }
          | '!' assertion { ANot $2 }
          | assertion "||" assertion { ADisj $1 $3 }
          | assertion "&&" assertion { AConj $1 $3 }
          | assertion "==>" assertion { AImpl $1 $3 }
          | "forall" list_plus(name) ',' assertion { AForall $2 $4 }
          | "exists" list_plus(name) ',' assertion { AExists $2 $4 }
          | '(' assertion ')' { $2 }
          {- | '(' assertion ')' { AParens $2} -}

stmt :: { Statement }
     : name ":=" arithExp ';' { Assign $1 $3 }
     | name ',' name ":=" arithExp ',' arithExp ';' { ParAssign $1 $3 $5 $7 }
     | name '[' arithExp ']' ":=" arithExp ';' { Write $1 $3 $6 }
     | "if" boolExp "then" block "else" block "end" { If $2 $4 $6 }
     | "if" boolExp "then" block "end" { If $2 $4 [] }
     | "while" boolExp list(inv) "do" block "end" { While $2 $3 $5 }

{

parseProg = parse1 . lexProg

parseError :: [Token] -> a
parseError _ = error "Parse error"

}