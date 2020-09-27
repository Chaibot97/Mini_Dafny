module Language where

import Data.List ( intercalate )
import Text.Printf ( printf )

type Name = String

data Operator = Add | Sub | Mul | Div | Mod
instance Show Operator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "mod"

data BoolFn = And | Or | Imply
instance Show BoolFn where
  show And = "and"
  show Or  = "or"
  show Imply = "=>"

data Order = Eq | Neq | Le | Ge | Lt | Gt
instance Show Order where
  show Eq = "="
  show Neq = "!="
  show Le = "<="
  show Ge = ">="
  show Lt = "<"
  show Gt = ">"

-- | Arithmetic expressions
data ArithExp = Num Int
              | Var Name
              | Read ArrExp ArithExp
              | BinOp Operator ArithExp ArithExp
  -- deriving (Show)
instance Show ArithExp where
  show (Num n) = show n
  show (Var x) = x
  show (Read a e) = printf "(select %s %s)" (show a) (show e)
  show (BinOp op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)

data ArrExp = Arr Name
            | Store ArrExp ArithExp ArithExp
instance Show ArrExp where
  show (Arr a) = a
  show (Store ae ei ev) = printf "(store %s %s %s)" (show ae) (show ei) (show ev)

-- | Comparisons of arithmetic expressions
data Comparison = Comp Order ArithExp ArithExp
instance Show Comparison where
  show (Comp ord e1 e2) = printf "(%s %s %s)" (show ord) (show e1) (show e2)

-- | Boolean expressions 
data BoolExp = BCmp Comparison
             | BNot BoolExp
             | BOp BoolFn BoolExp BoolExp
instance Show BoolExp where
  show (BCmp cmp) = show cmp
  show (BNot b) = "(not " ++ show b ++ ")"
  show (BOp op b1 b2) = printf "(%s %s %s)" (show op) (show b1) (show b2)

data Quantifier = Forall | Exists
instance Show Quantifier where
  show Forall = "forall"
  show Exists = "exists"
-- Assertion
data Assertion = ATrue | AFalse
               | ACmp Comparison
               | ANot Assertion
               | AOp BoolFn Assertion Assertion
               | AQuant Quantifier [Name] Assertion
               | AExists [Name] Assertion
instance Show Assertion where
  show ATrue = "true"
  show AFalse = "false"
  show (ACmp cmp) = show cmp
  show (ANot s) = "(not " ++ show s ++ ")"
  show (AOp op b1 b2) = printf "(%s %s %s)" (show op) (show b1) (show b2)
  show (AQuant q xs s) = printf "(%s (%s) %s)" (show q) (intercalate " " xs) (show s)

-- Program statements
type Block = [Statement]
data Statement = Assign Name ArithExp
               | ParAssign Name Name ArithExp ArithExp
               | Write Name ArithExp ArithExp
               | If BoolExp Block Block
               | While BoolExp [Assertion] Block
instance Show Statement where
  show s = intercalate "\n" (show_list s)

show_list :: Statement -> [String]
show_list (Assign x e) = [x ++ " := " ++ show e ++ ";"]
show_list (ParAssign x y ex ey) = [x ++ ", " ++ y ++ " := " ++ show ex ++ ", " ++ show ey ++ ";"]
show_list (Write a ei ev) = [show a ++ "[" ++ show ei ++ "]" ++ " := " ++ show ev ++ ";"]
show_list (If b c1 c2) =
  [ "if " ++ show b
  , "then" ] ++
    indent (showlist_block c1) ++
  [ "else" ] ++
    indent (showlist_block c2) ++
  [ "end" ]
show_list (While b invs c) =
  [ "while " ++ show b ] ++
    indent (prefix "inv " (map show invs)) ++
  [ "do" ] ++
    indent (showlist_block c) ++
  [ "end" ]


prefix :: String -> [String] -> [String]
prefix pre = map (\x -> pre ++ x)

indent :: [String] -> [String]
indent = prefix "  "

showlist_block :: Block -> [String]
showlist_block b = concat (map show_list b)

data Program = Program { name  :: Name
                       , pre   :: [Assertion]
                       , post  :: [Assertion]
                       , block :: Block
                       }
instance Show Program where
  show Program {name=name, pre=pre, post=post, block=block} =
    intercalate "\n" (
    [ "program " ++ name
    , intercalate "\n" (prefix "pre " (map show pre))
    , intercalate "\n" (prefix "post " (map show post))
    , "is" ] ++
    [ intercalate "\n" (indent (showlist_block block)) ] ++
    [ "end" ])

-- An intermediate language that does not have parallel assignment
type IMPBlock = [IMPStatement]
data IMPStatement = IAssign Name ArithExp
                  | IWrite Name ArithExp ArithExp
                  | IIf BoolExp IMPBlock IMPBlock
                  | IWhile BoolExp [Assertion] IMPBlock
                  deriving (Show)

-- instance Show IMPStatement where
  -- show s = intercalate "\n" (show_list s)

-- data IMPProgram = IMPProgram { iname  :: Name
--                              , ipre   :: [Assertion]
--                              , ipost  :: [Assertion]
--                              , iblock :: IMPBlock
--                              }
-- instance Show IMPProgram where
--   show IMPProgram {iname=name, ipre=pre, ipost=post, iblock=block} =
--     intercalate "\n" (
--     [ "program " ++ name
--     , intercalate "\n" (prefix "pre " (map show pre))
--     , intercalate "\n" (prefix "post " (map show post))
--     , "is" ] ++
--     [ intercalate "\n" (indent (showlist_block block)) ] ++
--     [ "end" ])

-- Loop-free guarded command language
type GCBlock = [GuardedCommand]
data GuardedCommand = GCAssert Assertion
                    | GCAssume Assertion
                    | GCHavoc  Name
                    | GCHavocArr Name
                    | GCChoice GCBlock GCBlock
                    | GCAssign Name ArithExp
                    | GCWrite Name ArithExp ArithExp
instance (Show GuardedCommand) where
  show gc = intercalate "\n" (gc_strlist gc)

gc_strlist :: GuardedCommand -> [String]
gc_strlist gc = case gc of
  GCAssert s   -> ["assert " ++ show s]
  GCAssume s   -> ["assume " ++ show s]
  GCHavoc v    -> ["havoc " ++ v]
  GCHavocArr a -> ["havoc array " ++ a]
  GCChoice c1 c2 -> ["choose"] ++ b1 ++ ["or"] ++ b2 where
    b1 = indent (gcblock_strlist c1)
    b2 = indent (gcblock_strlist c2)
  GCAssign x e -> ["assign " ++ x ++ " " ++ show e]
  GCWrite a ei ev -> [printf "write %s[%s] := %s" a (show ei) (show ev)]

gcblock_strlist :: [GuardedCommand] -> [String]
gcblock_strlist l = concat (map gc_strlist l)