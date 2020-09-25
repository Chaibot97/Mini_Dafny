module Language where

type Name = String

-- | Arithmetic expressions
data ArithExp = Num Int
              | Var Name
              | Read Name ArithExp
              | Add ArithExp ArithExp
              | Sub ArithExp ArithExp
              | Mul ArithExp ArithExp
              | Div ArithExp ArithExp
              | Mod ArithExp ArithExp
              | Parens ArithExp
              deriving (Show)

-- | Comparisons of arithmetic expressions
data Comparison = Eq ArithExp ArithExp
                | Neq ArithExp ArithExp
                | Le ArithExp ArithExp
                | Ge ArithExp ArithExp
                | Lt ArithExp ArithExp
                | Gt ArithExp ArithExp
                deriving (Show)

-- | Boolean expressions 
data BoolExp = BCmp Comparison
             | BNot BoolExp
             | BDisj BoolExp BoolExp
             | BConj BoolExp BoolExp
             | BParens BoolExp
             deriving (Show)

type Names = [Name]

-- Assertion
data Assertion = ACmp Comparison
                | ANot Assertion
                | ADisj Assertion Assertion
                | AConj Assertion Assertion
                | AInfer Assertion Assertion
                | AAll Names Assertion
                | AExt Names Assertion
                | AParens Assertion
                deriving (Show)

data Annotation = Pre Assertion
              | Post Assertion
              | Inv Assertion
              deriving (Show)

data Statement = Assign Name ArithExp
               | ParAssign Name Name ArithExp ArithExp
               | Write Name ArithExp ArithExp
               | If BoolExp Block Block
               | While BoolExp Annotations Block
               deriving (Show)

type Block = [Statement]

type Annotations = [Annotation]


-- type Program = (Name, Block)
-- PROG := "program" x [ "pre" ASSN ] [ "post" ASSN ] "is" BLOCK "end"
type Program = (Name, Annotations, Block)