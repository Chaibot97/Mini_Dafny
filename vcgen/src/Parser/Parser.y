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

prog :: { Program }
     : "program" name annotations "is" block "end" { ($2, $3, $5) }

arithExp :: { ArithExp }
         : int { Num $1 }
         | name { Var $1 }
         | '-' arithExp { Sub (Num 0) $2 }
         | name '[' arithExp ']' { Read $1 $3 }
         | arithExp '+' arithExp { Add $1 $3 }
         | arithExp '-' arithExp { Sub $1 $3 }
         | arithExp '*' arithExp { Mul $1 $3 }
         | arithExp '/' arithExp { Div $1 $3 }
         | arithExp '%' arithExp { Mod $1 $3 }
         | '(' arithExp ')'      { Parens $2 }

comp :: { Comparison }
     : arithExp '=' arithExp { Eq $1 $3 }
     | arithExp "!=" arithExp { Neq $1 $3 }
     | arithExp "<=" arithExp { Le $1 $3 }
     | arithExp ">=" arithExp { Ge $1 $3 }
     | arithExp '<' arithExp { Lt $1 $3 }
     | arithExp '>' arithExp { Gt $1 $3 }

boolExp :: { BoolExp }
        : comp { BCmp $1 }
        | '!' boolExp { BNot $2 }
        | boolExp "||" boolExp { BDisj $1 $3 }
        | boolExp "&&" boolExp { BConj $1 $3 }
        | '(' boolExp ')' { BParens $2 }

assertion :: { Assertion }
          : comp { ACmp $1 }
          | '!' assertion { ANot $2 }
          | assertion "||" assertion { ADisj $1 $3 }
          | assertion "&&" assertion { AConj $1 $3 }
          | assertion "==>" assertion { AInfer $1 $3 }
          | "forall" names ',' assertion { AAll $2 $4 }
          | "exists" names ',' assertion { AExt $2 $4 }
          | '(' assertion ')' { AParens $2}

stmt :: { Statement }
     : name ":=" arithExp ';' { Assign $1 $3 }
     | name ',' name ":=" arithExp ',' arithExp ';' { ParAssign $1 $3 $5 $7 }
     | name '[' arithExp ']' ":=" arithExp ';' { Write $1 $3 $6 }
     | "if" boolExp "then" block "else" block "end" { If $2 $4 $6 }
     | "if" boolExp "then" block "end" { If $2 $4 [] }
     | "while" boolExp annotations "do" block "end" { While $2 $3 $5 }


annotation :: { Annotation }
          : "pre" assertion {Pre $2}
          | "post" assertion {Post $2}
          | "inv" assertion {Inv $2}

names :: {Names}
     : names_rev { reverse $1 }

names_rev :: { Names }
          : name { [$1] }
          | names_rev name {$2:$1}


annotations :: { Annotations }
      : annotations_rev { reverse $1 }

annotations_rev :: { Annotations }
          : annotation { [$1] }
          | annotations_rev annotation {$2:$1}



block :: { Block }
      : block_rev { reverse $1 }

block_rev :: { Block }
          : stmt { [$1] }
          | block_rev stmt {$2:$1}

{

parseProg = parse1 . lexProg

parseError :: [Token] -> a
parseError _ = error "Parse error"

} 