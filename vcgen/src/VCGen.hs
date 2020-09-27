module Main where

import Language
import Parser.Parser

import Data.List ( (\\), intercalate )
import qualified Data.Set as S ( Set, empty, singleton, insert, union, difference, map, fromList, toList, member ) 
import Text.Printf ( printf )
import System.Environment

import Debug.Trace

-- A stream of fresh variables
fresh = map (\i -> "_t" ++ show i) [1..]

nuu :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
nuu (xs, ys) (xs', ys') = (xs++xs', ys++ys')

nuu_s :: (Ord a, Ord b) => (S.Set a, S.Set b) -> (S.Set a, S.Set b) -> (S.Set a, S.Set b)
nuu_s (s1, s2) (s1', s2') = (S.union s1 s1', S.union s2 s2')

-- Given a stream of fresh names, consume the first name and
-- return (a, rest of the names)
--
-- Inspired by https://stackoverflow.com/a/44802556
-- TODO: Use State Monad and traverse
type NeedFreshNames a = [Name] -> (a, [Name])

-- Transform surface language into the intermediate language
stmt_to_imp :: Statement -> NeedFreshNames [IMPStatement]
stmt_to_imp (Assign x e) fv = ([IAssign x e], fv)
stmt_to_imp (ParAssign x y ex ey) fv =
    ([ IAssign vx ex, IAssign vy ey, IAssign x (Var vx), IAssign y (Var vy) ], fv'') where
        (vx : fv') = fv
        (vy : fv'') = fv'
stmt_to_imp (Write a ei ev) fv =
    ([ IWrite a ei ev ], fv) 
    -- Alternatively (and perhaps a safer way) we can
    -- create temporary variables to store values of ei and ev
    -- ([ IAssign i ei, IAssign v ev, IWrite a (Var i) (Var v)], fv'') where
    --     (i : fv') = fv
    --     (v : fv'') = fv'
stmt_to_imp (If b c1 c2) fv =
    ([IIf b gc1 gc2], fv'') where
        (gc1, fv') = prog_to_imp c1 fv
        (gc2, fv'') = prog_to_imp c2 fv'
stmt_to_imp (While e inv c) fv =
    ([IWhile e inv gc], fv') where
        (gc, fv') = prog_to_imp c fv

fold_fv :: (a -> NeedFreshNames [b]) -> [a] -> NeedFreshNames [b]
fold_fv f [] fv = ([], fv)
fold_fv f (x:xs) fv = (y ++ ys, fv'') where
  (y, fv') = f x fv
  (ys, fv'') = fold_fv f xs fv'

prog_to_imp :: Block -> NeedFreshNames [IMPStatement]
prog_to_imp = fold_fv stmt_to_imp

-- Substitute ArithExp new for Var old in ArithExp e, that is, e[new\old]
subst_v_in_arith :: ArithExp -> Name -> ArithExp -> ArithExp
subst_v_in_arith new old e = case e of
    Num n -> e
    Var x -> if x == old then new else e
    BinOp op e1 e2 -> BinOp op (subst_v_in_arith new old e1) (subst_v_in_arith new old e2)
    Read r ei      -> Read (subst_v_in_arr new old r) (subst_v_in_arith new old ei)

-- Substitute ArithExp new for Var x in ArrExp r, that is, r[new\old]
subst_v_in_arr :: ArithExp -> Name -> ArrExp -> ArrExp
subst_v_in_arr new old r = case r of
    Arr a -> r
    Store t ei ev -> Store t' ei' ev' where
        t'  = subst_v_in_arr new old t
        ei' = subst_v_in_arith new old ei
        ev' = subst_v_in_arith new old ev

-- Substitute ArrExp new for Arr old in ArithExp e, that is, e[new\old]
subst_a_in_arith :: ArrExp -> Name -> ArithExp -> ArithExp
subst_a_in_arith new old e = case e of
    Num n -> e
    Var x -> e
    Read r ei -> Read (subst_a_in_arr new old r) (subst_a_in_arith new old ei)
    BinOp op e1 e2 -> BinOp op (subst_a_in_arith new old e1) (subst_a_in_arith new old e2)

-- Substitute ArrExp new for Arr x in ArrExp r, that is, r[new\old]
subst_a_in_arr :: ArrExp -> Name -> ArrExp -> ArrExp
subst_a_in_arr new old r = case r of
    Arr a -> if a == old then new else r
    Store t ei ev -> Store t' ei' ev' where
        t'  = subst_a_in_arr new old t
        ei' = subst_a_in_arith new old ei
        ev' = subst_a_in_arith new old ev

fmap_cmp :: (ArithExp -> ArithExp) -> Comparison -> Comparison
fmap_cmp f (Comp ord e1 e2) = Comp ord (f e1) (f e2)

subst_v_in_cmp new old = fmap_cmp (subst_v_in_arith new old)
subst_a_in_cmp new old =  fmap_cmp (subst_a_in_arith new old)

subst_assert :: Name -> (Name -> ArithExp -> ArithExp) -> Assertion -> Assertion
subst_assert x f s = case s of
    ACmp cmp -> ACmp (fmap_cmp (f x) cmp)
    ANot s   -> ANot (subst_assert x f s)
    AOp op s1 s2 -> AOp op (subst_assert x f s1) (subst_assert x f s2)
    AQuant q xs s' -> if elem x xs then s else AQuant q xs (subst_assert x f s')
    _ -> s

subst_v_in_assert new old = subst_assert old (subst_v_in_arith new)
subst_a_in_assert new old = subst_assert old (subst_a_in_arith new)

modified :: IMPStatement -> ([String], [String])
modified (IAssign x _)  = ([], [x])
modified (IWrite a _ _) = ([a], [])
modified (IIf _ c1 c2)  = nuu (block_modified c1) (block_modified c2)
modified (IWhile _ _ c) = block_modified c

block_modified :: IMPBlock -> ([String], [String])
block_modified [] = ([], [])
block_modified (hd:tl) = nuu (modified hd) (block_modified tl)

conj :: [Assertion] -> Assertion
conj [] = ATrue
conj [s] = s
conj (hd:tl) = (AOp And hd (conj tl))

bool_to_assert :: BoolExp -> Assertion
bool_to_assert (BCmp cmp) = ACmp cmp
bool_to_assert (BNot b) = ANot (bool_to_assert b)
bool_to_assert (BOp op b1 b2) = AOp op (bool_to_assert b1) (bool_to_assert b2)
bool_to_assert (BOp op b1 b2) = AOp op (bool_to_assert b1) (bool_to_assert b2)

imp_to_gc :: IMPStatement -> NeedFreshNames [GuardedCommand]
imp_to_gc (IAssign x e) fv =
    -- ([ GCAssign x e], fv)
    ([ GCAssume (ACmp (Comp Eq v_xf v_x))
     , GCHavoc x
     , GCAssume (ACmp (Comp Eq v_x (subst_v_in_arith v_xf x e))) ], fv') where
        (xf : fv') = fv
        v_xf = Var xf
        v_x  = Var x
imp_to_gc (IWrite a ei ev) fv0 =
      -- forall i, a'[i] == a[i]
    ([ GCAssume (AQuant Forall [i] (ACmp (Comp Eq (Read (Arr a') (Var i)) (Read (Arr a) (Var i)))))
      -- havoc a
     , GCHavocArr a
      -- forall j, a[j] = (store a' ei' ev')[j], where ei' = ei[a'/a], ev' = ev[a'/a]
     , GCAssume (AQuant Forall [j]
        (ACmp (Comp Eq
          (Read (Arr a) (Var j))
          (Read (Store (Arr a') ei' ev') (Var j))))) ], fv3) where
        a' : fv1 = fv0
        i : fv2 = fv1
        j : fv3 = fv2
        ei' = subst_a_in_arith (Arr a') a ei
        ev' = subst_a_in_arith (Arr a') a ev
imp_to_gc (IIf b c1 c2) fv =
    ([ GCChoice (GCAssume b' : gc1) (GCAssume (ANot b') : gc2) ], fv'') where
        (gc1, fv')  = imp_prog_to_gc c1 fv
        (gc2, fv'') = imp_prog_to_gc c2 fv'
        b' = bool_to_assert b
imp_to_gc (IWhile b invs c) fv = 
    ([ GCAssert inv ] ++ havoc_int ++ havoc_arr ++ [ GCAssume inv ] ++
        [ GCChoice 
            ((GCAssume b') : gc ++ [ GCAssert inv, GCAssume AFalse ])
            [GCAssume (ANot b')] ], fv') where
        inv = conj invs
        b' = bool_to_assert b
        (rs, vs) = block_modified c
        havoc_int = map GCHavoc vs
        havoc_arr = map GCHavocArr rs
        (gc, fv') = imp_prog_to_gc c fv

imp_prog_to_gc :: [IMPStatement] -> NeedFreshNames [GuardedCommand]
imp_prog_to_gc = fold_fv imp_to_gc

wp :: GuardedCommand -> Assertion -> NeedFreshNames Assertion
wp gc q fv =
  -- Uncomment the following line to enable debug messages
  -- trace (intercalate "\n" ["Command:", show gc, "Message:", msg, "WP:", show qq, "Q:", show q, "\n"])
  (q', fv') where
    (q', fv', msg) = case gc of
      GCAssert s -> case q of
          ATrue -> (s, fv, "right operand is true")
          _ -> (q', fv', msg) where
              (q', fv') = (AOp And s q, fv)
              msg = ("&& " ++ show s)
      GCAssume AFalse -> (ATrue, fv, "falsehood implies everything")
      GCAssume s -> (q', fv', msg) where
          (q', fv') = (AOp Imply s q, fv)
          msg = ("=> " ++ show s)
      GCHavoc x -> (q', fv', msg) where
          v : fv' = fv
          q' = subst_v_in_assert (Var v) x q
          msg = printf "replace %s with %s in\n%s\n" x v (show q) ++ (show q')
      GCHavocArr a -> (q', fv', msg) where
          a' : fv' = fv
          q' = subst_a_in_assert (Arr a') a q
          msg = printf "replace %s with %s in\n  %s\nwhich becomes\n  %s" a a' (show q) (show q')
      GCChoice gc1 gc2 -> (q', fv'', msg) where
          (q1, fv') = wp_block gc1 q fv
          (q2, fv'') = wp_block gc2 q fv'
          q' = AOp And q1 q2
          msg = printf "left branch\n" ++ (show q1) ++ "\nright branch\n" ++ (show q2)
      -- GCAssign x e -> (subst_v_in_assert e x q, fv, "assign")
      -- GCWrite a ei ev -> (subst_a_in_assert (Store (Arr a) ei ev) a q, fv, "write")

split_last :: [a] -> ([a], a)
split_last l = case reverse l of
  [] -> error "list must be non-empty"
  hd:tl -> (reverse tl, hd)

wp_block :: [GuardedCommand] -> Assertion -> NeedFreshNames Assertion
wp_block [] q fv = (q, fv)
wp_block (hd:tl) q0 fv0 = (q2, fv2) where
  (q1, fv1) = wp_block tl q0 fv0
  (q2, fv2) = wp hd q1 fv1

-- extract array variables and integer variables from ArithExp
names :: ArithExp -> (S.Set Name, S.Set Name)
names (Num _) = (S.empty, S.empty)
names (Var x) = (S.empty, S.singleton x)
names (Read r e) = nuu_s (names_arr r) (names e)
names (BinOp _ e1 e2) = nuu_s (names e1) (names e2)

-- extract array variables and integer variables from ArrExp
names_arr :: ArrExp -> (S.Set Name, S.Set Name)
names_arr (Arr a) = (S.singleton a, S.empty)
names_arr (Store r ei ev) = nuu_s (names_arr r) (nuu_s (names ei) (names ev))

-- Free variables of types array and int
free :: Assertion -> (S.Set Name, S.Set Name)
free (ACmp (Comp _ e1 e2)) = nuu_s (names e1) (names e2)
free (ANot s) = free s
free (AOp _ s1 s2) = nuu_s (free s1) (free s2)
free (AQuant _ xs s) = (S.difference r xs_set, S.difference v xs_set) where
    (r, v) = free s
    xs_set = S.fromList xs
free _ = (S.empty, S.empty)

-- show ArithExp is already in smt syntax
arith_to_smt :: ArithExp -> String
arith_to_smt = show

-- show ArrExp is already in smt syntax
arr_to_smt :: ArrExp -> String
arr_to_smt = show

-- Could have used show, but smt syntax does not have !=
cmp_to_smt :: Comparison -> String
cmp_to_smt (Comp Neq e1 e2) = printf "(not (= %s %s))" (arith_to_smt e1) (arith_to_smt e2)
cmp_to_smt (Comp ord e1 e2) = printf "(%s %s %s)" (show ord) (arith_to_smt e1) (arith_to_smt e2)

assert_to_smt :: Assertion -> String
assert_to_smt ATrue = "true"
assert_to_smt AFalse = "false"
assert_to_smt (ACmp cmp) = cmp_to_smt cmp
assert_to_smt (ANot s) = "(not " ++ assert_to_smt s ++ ")"
assert_to_smt (AOp op s1 s2) = printf "(%s %s %s)" op_smt s1_smt s2_smt where
    op_smt = case op of
        Or -> "or" 
        And -> "and"
        Imply -> "=>"
    s1_smt = assert_to_smt s1
    s2_smt = assert_to_smt s2
assert_to_smt (AQuant q [] s) = assert_to_smt s
assert_to_smt (AQuant q (x:xs) s) = 
    let s_xs = AQuant q xs s
        (arrs, ints) = free s_xs in
    if elem x ints
    then printf "(forall ((%s %s)) %s)" x "Int" (assert_to_smt s_xs)
    else if elem x arrs
        then printf "(forall ((%s %s)) %s)" x "(Array Int Int)" (assert_to_smt s_xs)
        else trace (show (AQuant q (x:xs) s)) error (x ++ " not found")

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as) 
    let
        p = parseProg prog
        fv0 = fresh -- keep around the stream of fresh names
        (imp, fv1) = prog_to_imp (block p) fv0 -- eliminate parallel assignment
        (gc, fv2) = imp_prog_to_gc imp fv1
        (weakest, fv3) = wp_block gc (conj $ post p) fv2
        vc = (ANot (AOp Imply (conj $ pre p) weakest))
        (arrs, ints) = free vc -- free variables of types array and int
        declare_arr = printf "(declare-const %s (Array Int Int))"
        declare_int = printf "(declare-const %s Int)"
        arrs_str = S.toList $ S.map declare_arr arrs
        ints_str = S.toList $ S.map declare_int ints
        vc_assert = printf "(assert %s)" (assert_to_smt vc)
        vc_str = intercalate "\n" (arrs_str ++ ints_str ++ [vc_assert, "(check-sat)"])
    putStrLn vc_str