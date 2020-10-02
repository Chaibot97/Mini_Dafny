module Main where

import Language
import Parser.Parser

import Data.List ( (\\), intercalate, filter, notElem )
import qualified Data.Set as S ( Set, empty, singleton, insert, union, difference, map, fromList, toList, member ) 
import Text.Printf ( printf )
import System.Environment

import Debug.Trace

-- A stream of fresh variables
fresh = map (\i -> "_t" ++ show i) [1..]

-- Given a stream of fresh names, consume the first name and
-- return (a, rest of the names)
-- Inspired by https://stackoverflow.com/a/44802556
-- TODO: Implement using Traversable
type NeedFreshNames a = [Name] -> (a, [Name])

fold_fv :: (a -> NeedFreshNames [b]) -> [a] -> NeedFreshNames [b]
fold_fv f [] fv = ([], fv)
fold_fv f (x:xs) fv = (y ++ ys, fv'') where
  (y, fv') = f x fv
  (ys, fv'') = fold_fv f xs fv'

-- Substitute AExp |new| for name |old| of type |t| in AExp |e|, that is, e[new\old]
subst_aexp :: AExp -> Typed -> AExp -> AExp
subst_aexp new (old,t) e =
  let subst = subst_aexp new (old,t) in
  case e of
    Num n -> e
    Var x -> case t of {Int -> if x == old then new else e; IntArr -> e}
    Arr x -> case t of {IntArr -> if x == old then new else e; Int -> e}
    BinOp op e1 e2 -> BinOp op (subst e1) (subst e2)
    Read ea ei     -> Read (subst ea) (subst ei)
    Store ea ei ev -> Store (subst ea) (subst ei) (subst ev)

subst_cmp :: AExp -> Typed -> Comparison -> Comparison
subst_cmp new (old,t) (Comp ord e1 e2) = let subst = subst_aexp new (old,t) in
  Comp ord (subst e1) (subst e2)

subst_bexp :: AExp -> Typed -> BExp -> BExp
subst_bexp new (old,t) b = let subst = subst_bexp new (old,t) in
  case b of
    BCmp c -> BCmp (subst_cmp new (old,t) c)
    BNot b' -> BNot (subst b')
    BBinOp op b1 b2 -> BBinOp op (subst b1) (subst b2)

subst_assert :: AExp -> Typed -> Assertion -> Assertion
subst_assert new (old,t) s = let subst = subst_assert new (old,t) in
  case s of
    ACmp c -> ACmp (subst_cmp new (old,t) c)
    ANot s' -> ANot (subst s')
    ABinOp op b1 b2 -> ABinOp op (subst b1) (subst b2)
    AQ q xs s' -> AQ q xs s'' where
      s'' = case t of 
        Int -> if elem old xs then s' else (subst s')
        IntArr -> (subst s')
    _ -> s

modified :: Statement -> S.Set Typed
modified (Assign x _)  = S.singleton (x, Int)
modified (Write a _ _) = S.singleton (a, IntArr)
modified (If _ c1 c2)  = S.union (block_modified c1) (block_modified c2)
modified (While _ _ c) = block_modified c
modified Skip = S.empty

block_modified :: Block -> S.Set Typed
block_modified = foldl (\acc s -> S.union acc (modified s)) S.empty

conj :: [Assertion] -> Assertion
conj [] = ATrue
conj [s] = s
conj (hd:tl) = ABinOp And hd (conj tl)

bexp_to_assert :: BExp -> Assertion
bexp_to_assert (BCmp c) = ACmp c
bexp_to_assert (BNot b) = ANot (bexp_to_assert b)
bexp_to_assert (BBinOp op b1 b2) = ABinOp op b1' b2' where
  b1' = bexp_to_assert b1
  b2' = bexp_to_assert b2

stmt_to_gc :: Statement -> NeedFreshNames GCBlock
stmt_to_gc (Assign x e) fv0 =
    ([ GCAssume (ACmp (Comp Eq v_xf v_x))
     , GCHavoc (x, Int)
     , GCAssume (ACmp (Comp Eq v_x (subst_aexp v_xf (x,Int) e))) ], fv1) where
        (xf : fv1) = fv0
        v_xf = Var xf
        v_x  = Var x
stmt_to_gc (Write a (Var ind) (Var val)) fv0 =
    ([ GCAssume (arr_equal (Arr a') (Arr a) i)
     , GCHavoc (a, IntArr)
     , GCAssume (arr_equal (Arr a) store j) ], fv3) where
        a' : fv1 = fv0
        i : fv2 = fv1
        j : fv3 = fv2
        store = Store (Arr a') (Var ind) (Var val)
        arr_equal a1 a2 i = AQ Forall [i] (ACmp (Comp Eq (Read a1 (Var i)) (Read a2 (Var i))))
stmt_to_gc (Write a ei ev) fv0 = 
    prog_to_gc [ Assign ind ei
     , Assign val ev
     , Write a (Var ind) (Var val)] fv2 where
       ind : fv1 = fv0
       val : fv2 = fv1
stmt_to_gc (ParAssign x1 x2 e1 e2) fv0 = (gc, fv3) where
  v1 : fv1 = fv0
  v2 : fv2 = fv1
  (gc, fv3) = prog_to_gc [ Assign v1 e1
              , Assign v2 e2
              , Assign x1 (Var v1)
              , Assign x2 (Var v2) ] fv2
stmt_to_gc (If b c1 c2) fv =
    ([ GCChoice (GCAssume bs : gc1) (GCAssume (ANot bs) : gc2) ], fv'') where
        (gc1, fv')  = prog_to_gc c1 fv
        (gc2, fv'') = prog_to_gc c2 fv'
        bs = bexp_to_assert b
stmt_to_gc (While b invs c) fv = 
    ([ GCAssert inv ] ++ havoc ++ [ GCAssume inv ] ++
        [ GCChoice 
            ((GCAssume bs) : gc ++ [ GCAssert inv, GCAssume AFalse ])
            [GCAssume (ANot bs)] ], fv') where
        bs = bexp_to_assert b
        inv = conj invs
        xs = block_modified c
        havoc = map GCHavoc (S.toList xs)
        (gc, fv') = prog_to_gc c fv
stmt_to_gc Skip fv = ([], fv)

prog_to_gc :: Block -> NeedFreshNames GCBlock
prog_to_gc = fold_fv stmt_to_gc

wp :: GuardedCommand -> Assertion -> NeedFreshNames Assertion
wp gc q fv =
  -- Uncomment the following line to enable debug messages
  -- trace (intercalate "\n" ["Command:", show gc, "Message:", msg, "WP:", show q', "Q:", show q, "\n"])
  (q', fv') where
    (q', fv', msg) = case gc of
      GCAssert s -> case q of
          ATrue -> (s, fv, "right operand is true")
          _ -> (q', fv', msg) where
              (q', fv') = (ABinOp And s q, fv)
              msg = ("&& " ++ show s)
      GCAssume AFalse -> (ATrue, fv, "falsehood implies everything")
      GCAssume s -> (q', fv', msg) where
          (q', fv') = (ABinOp Imply s q, fv)
          msg = ("=> " ++ show s)
      GCHavoc (x,t) -> (q', fv', msg) where
          v : fv' = fv
          v_e = case t of {Int -> Var v; IntArr -> Arr v}
          q' = subst_assert v_e (x,t) q
          msg = printf "replace %s with %s in\n%s\n" x v (show q) ++ (show q')
      GCChoice gc1 gc2 -> (q', fv'', msg) where
          (q1, fv') = wp_block gc1 q fv
          (q2, fv'') = wp_block gc2 q fv'
          q' = ABinOp And q1 q2
          msg = printf "left branch\n" ++ (show q1) ++ "\nright branch\n" ++ (show q2)

split_last :: [a] -> ([a], a)
split_last l = case reverse l of
  [] -> error "list must be non-empty"
  hd:tl -> (reverse tl, hd)

wp_block :: GCBlock -> Assertion -> NeedFreshNames Assertion
wp_block [] q fv = (q, fv)
wp_block (hd:tl) q0 fv0 = (q2, fv2) where
  (q1, fv1) = wp_block tl q0 fv0
  (q2, fv2) = wp hd q1 fv1

infer_a :: AExp -> Typing
infer_a (Num _) = []
infer_a (Var x) = [(x, Int)]
infer_a (Arr x) = [(x, IntArr)]
infer_a (BinOp _ e1 e2) = infer_a e1 ++ infer_a e2
infer_a (Read ea ei) = infer_a ea ++ infer_a ei
infer_a (Store ea ei ev) = infer_a ea ++ infer_a ei ++ infer_a ev

infer_c :: Comparison -> Typing
infer_c (Comp _ e1 e2) = infer_a e1 ++ infer_a e2

infer_b :: BExp -> Typing
infer_b (BCmp c) = infer_c c
infer_b (BNot b) = infer_b b
infer_b (BBinOp _ b1 b2) = infer_b b1 ++ infer_b b2
infer_b _ = []

infer :: Assertion -> Typing
infer (ACmp c) = infer_c c
infer (ANot s) = infer s
infer (ABinOp _ s1 s2) = infer s1 ++ infer s2
infer (AQ _ xs s) = filter not_captured (infer s) where
  not_captured (y,Int) = notElem y xs
  not_captured _ = True
infer _ = []

-- show AExp is already in smt syntax
aexp_to_smt :: AExp -> String
aexp_to_smt = show

-- smt syntax does not have !=
cmp_to_smt :: Comparison -> String
cmp_to_smt (Comp Neq e1 e2) = printf "(not (= %s %s))" (aexp_to_smt e1) (aexp_to_smt e2)
cmp_to_smt (Comp ord e1 e2) = printf "(%s %s %s)" (show ord) (aexp_to_smt e1) (aexp_to_smt e2)

bexp_to_smt :: BExp -> String
bexp_to_smt (BCmp c) = cmp_to_smt c
bexp_to_smt (BNot b) = "(not " ++ bexp_to_smt b ++ ")"
bexp_to_smt (BBinOp op b1 b2) = printf "(%s %s %s)" (show op) (bexp_to_smt b1) (bexp_to_smt b2)


typing_to_str :: Typing -> String
typing_to_str ts = intercalate " " (map (\(x,t) -> printf "(%s %s)" x (show t)) ts)

to_smt :: Assertion -> String
to_smt (ACmp c) = cmp_to_smt c
to_smt (ANot s) = "(not " ++ to_smt s ++ ")"
to_smt (ABinOp op s1 s2) = printf "(%s %s %s)" (show op) (to_smt s1) (to_smt s2)
to_smt (AQ q xs s) = printf "(%s (%s) %s)" (show q) xts (to_smt s) where
  xts = typing_to_str (zip xs (repeat Int))
to_smt s = show s


main :: IO ()
main = do
  as <- getArgs
  prog <- readFile (head as) 
  let
    p = parseProg prog
    fv0 = fresh -- keep around the stream of fresh names
    (gc, fv1) = prog_to_gc (block p) fv0
    (weakest, fv2) = wp_block gc (conj $ post p) fv1
    vc = (ANot (ABinOp Imply (conj $ pre p) weakest))
    ts = S.toList (S.fromList (infer vc))
    declare_str :: (Name, Type) -> String
    declare_str (x,t) = printf "(declare-const %s %s)" x (show t)
    ds = map declare_str ts
  -- putStrLn (intercalate "\n" (gcblock_strlist gc))
  putStrLn "(set-logic ALL)"
  putStrLn (intercalate "\n" ds)
  putStrLn ("(assert " ++  to_smt vc ++ ")")
  putStrLn "(check-sat)"