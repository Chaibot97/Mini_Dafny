module Language where

type Name = String

data Operator = Add | Sub | Mul | Div | Mod deriving (Show)
data Order = Eq | Neq | Le | Ge | Lt | Gt deriving (Show)

-- | Arithmetic expressions
data ArithExp = Num Int
              | Var Name
              | Read  Name ArithExp
              | Store Name ArithExp ArithExp
              | BinOp Operator ArithExp ArithExp
            --   | Parens ArithExp
              deriving (Show)

-- | Comparisons of arithmetic expressions
data Comparison = Comp Order ArithExp ArithExp
                deriving (Show)

-- | Boolean expressions 
data BoolExp = BCmp  Comparison
             | BNot  BoolExp
             | BDisj BoolExp BoolExp
             | BConj BoolExp BoolExp
            --  | BParens BoolExp
             deriving (Show)

type Names = [Name]

-- Assertion
data Assertion = ACmp Comparison
               | ANot Assertion
               | ADisj Assertion Assertion
               | AConj Assertion Assertion
               | AImpl Assertion Assertion
               | AForall Names Assertion
               | AExists Names Assertion
            --    | AParens Assertion
               deriving (Show)

data Statement = Assign Name ArithExp
               | ParAssign Name Name ArithExp ArithExp
               | Write Name ArithExp ArithExp
               | If BoolExp Block Block
               | While BoolExp [Assertion] Block
               deriving (Show)

type Block = [Statement]

data Program = Program { name  :: Name
                       , pre   :: [Assertion]
                       , post  :: [Assertion]
                       , block :: Block
                       } deriving (Show)