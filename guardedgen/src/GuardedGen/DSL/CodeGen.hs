module GuardedGen.DSL.CodeGen
  ( genLHAnnotation
  , genLHAnnotations
  ) where

import Data.List  (intercalate, nub)
import GuardedGen.DSL.AST

-- ============================================================
-- AST → Liquid Haskell 注釈文字列の生成
-- ============================================================

genLHAnnotation :: SpecDecl -> String
genLHAnnotation spec = unlines
  [ "{-@ " ++ specName spec ++ " :: "
      ++ genLHTypeSig (specTypeSig spec) (specClauses spec)
      ++ " @-}"
  , specName spec ++ " :: " ++ genHaskellTypeSig (specTypeSig spec)
  , specName spec ++ " = undefined"
  ]

genLHAnnotations :: [SpecDecl] -> String
genLHAnnotations = concatMap (\s -> genLHAnnotation s ++ "\n")

-- ============================================================
-- 型シグネチャの生成
-- ============================================================

genLHTypeSig :: TypeSig -> [Clause] -> String
genLHTypeSig (TypeSig args result) clauses =
  let
    requires  = [e | Requires e <- clauses]
    ensures   = [e | Ensures  e <- clauses]

    -- 1. 引数名を決定（clauses内の自由変数を優先）
    argNames  = assignArgNames clauses args

    -- 2. `input` の置換先: 最初の非関数型引数
    inputName = findInputArgName argNames args

    -- 3. 置換マップ
    subst     = [("input", inputName), ("output", "v")]

    -- 4. 引数部分（最後の引数に requires を付加）
    namedArgs = zip argNames args
    argsStr   = genArgsStr namedArgs requires subst

    -- 5. 戻り値部分（ensures を refined type に）
    resultStr = genResultType result ensures subst

  in argsStr ++ resultStr

-- ============================================================
-- 引数名の決定
-- ============================================================

-- | clauses 内の自由変数を先頭に使い、余りはデフォルト名で埋める
assignArgNames :: [Clause] -> [Type] -> [String]
assignArgNames clauses types =
  let reserved = ["output", "input"]
      freeVars = nub $ concatMap (collectFreeVars reserved . clauseExpr) clauses
      defaults = defaultArgNames types
      -- freeVars を先頭に割り当て、不足分は defaults で補う
      padded   = freeVars ++ drop (length freeVars) defaults
  in take (length types) padded

-- | 句から式を取り出す
clauseExpr :: Clause -> Expr
clauseExpr (Ensures  e) = e
clauseExpr (Requires e) = e

-- | 式から自由変数（予約語以外の EVar）を収集
collectFreeVars :: [String] -> Expr -> [String]
collectFreeVars reserved (EVar x)
  | x `elem` reserved = []
  | otherwise          = [x]
collectFreeVars reserved (EApp _ args)    = concatMap (collectFreeVars reserved) args
collectFreeVars reserved (EBinOp _ l r)   = collectFreeVars reserved l
                                           ++ collectFreeVars reserved r
collectFreeVars reserved (ENeg e)         = collectFreeVars reserved e
collectFreeVars _        _                = []

-- | 型ベースのデフォルト引数名
defaultArgNames :: [Type] -> [String]
defaultArgNames types = snd $ foldl step (counters, []) types
  where
    counters = (0::Int, 0::Int, 0::Int)  -- (Int-idx, list-idx, fun-idx)
    step ((ic, lc, fc), acc) t = case t of
      TInt           -> ((ic+1, lc,   fc),   acc ++ [intNames !! ic])
      TBool          -> ((ic,   lc,   fc),   acc ++ ["p"])
      TList _        -> ((ic,   lc+1, fc),   acc ++ [listName lc])
      TFun _ _       -> ((ic,   lc,   fc+1), acc ++ [funName fc])
      TRefined n _ _ -> ((ic,   lc,   fc),   acc ++ [n])
    intNames     = ["x", "y", "z", "w", "a", "b", "c"]
    listName 0   = "xs"; listName 1 = "ys"; listName _ = "zs"
    funName  0   = "f";  funName  1 = "g";  funName  _ = "h"

-- | `input` の置換先: 最初の非関数型引数名
findInputArgName :: [String] -> [Type] -> String
findInputArgName names types =
  case filter (not . isFun . snd) (zip names types) of
    ((n, _):_) -> n
    []         -> head names
  where
    isFun (TFun _ _) = True
    isFun _          = False

-- ============================================================
-- 引数部分の文字列生成
-- ============================================================

-- | 引数リストを生成。最後の引数に requires を付加
genArgsStr :: [(String, Type)] -> [Expr] -> [(String, String)] -> String
genArgsStr [] _ _  = ""
genArgsStr namedArgs requires subst =
  let n          = length namedArgs
      (front, [(lastName, lastType)]) = splitAt (n-1) namedArgs
      frontStr   = concatMap (\(nm, t) -> nm ++ ":" ++ genLHType t ++ " -> ") front
      lastStr    = genLastArg lastName lastType requires subst
  in frontStr ++ lastStr

-- | 最後の引数: requires があれば refined type に
genLastArg :: String -> Type -> [Expr] -> [(String, String)] -> String
genLastArg name t [] _     = name ++ ":" ++ genLHType t ++ " -> "
genLastArg name t reqs subst =
  let substReqs = map (substExpr subst) reqs
      predStr   = intercalate " && " (map genExpr substReqs)
  in name ++ ":{" ++ name ++ ":" ++ genLHTypeBase t ++ " | " ++ predStr ++ "} -> "

-- ============================================================
-- 戻り値の refined type 生成
-- ============================================================

genResultType :: Type -> [Expr] -> [(String, String)] -> String
genResultType t [] _ = genLHType t
genResultType t ensures subst =
  let substEnsures = map (substExpr subst) ensures
      predStr      = intercalate " && " (map genExpr substEnsures)
  in "{v:" ++ genLHTypeBase t ++ " | " ++ predStr ++ "}"

-- ============================================================
-- 式の変数置換
-- ============================================================

substExpr :: [(String, String)] -> Expr -> Expr
substExpr subst (EVar x)        = EVar (maybe x id (lookup x subst))
substExpr subst (EApp f args)   = EApp f (map (substExpr subst) args)
substExpr subst (EBinOp op l r) = EBinOp op (substExpr subst l) (substExpr subst r)
substExpr subst (ENeg e)        = ENeg (substExpr subst e)
substExpr _     e               = e

-- ============================================================
-- 型の文字列化
-- ============================================================

genLHType :: Type -> String
genLHType TInt             = "Int"
genLHType TBool            = "Bool"
genLHType (TList t)        = "[" ++ genLHType t ++ "]"
genLHType (TFun a b)       = "(" ++ genLHType a ++ " -> " ++ genLHType b ++ ")"
genLHType (TRefined n t e) = "{" ++ n ++ ":" ++ genLHTypeBase t ++ " | " ++ genExpr e ++ "}"

genLHTypeBase :: Type -> String
genLHTypeBase (TRefined _ t _) = genLHTypeBase t
genLHTypeBase t                = genLHType t

genHaskellTypeSig :: TypeSig -> String
genHaskellTypeSig (TypeSig args result) =
  intercalate " -> " (map genHaskellType (args ++ [result]))

genHaskellType :: Type -> String
genHaskellType TInt             = "Int"
genHaskellType TBool            = "Bool"
genHaskellType (TList t)        = "[" ++ genHaskellType t ++ "]"
genHaskellType (TFun a b)       = "(" ++ genHaskellType a ++ " -> " ++ genHaskellType b ++ ")"
genHaskellType (TRefined _ t _) = genHaskellType t

-- ============================================================
-- 式の文字列化（LH述語に変換）
-- ============================================================

genExpr :: Expr -> String
genExpr (EVar x)        = x
genExpr (ELit n)        = show n
genExpr (EApp f args)   = translateApp f (map genExpr args)
genExpr (EBinOp op l r) = "(" ++ genExpr l ++ " " ++ genBinOp op ++ " " ++ genExpr r ++ ")"
genExpr (ENeg e)        = "(0 - " ++ genExpr e ++ ")"

translateApp :: String -> [String] -> String
translateApp "len"         [xs]     = "llen " ++ xs
translateApp "sorted"      [xs]     = "isSorted " ++ xs
translateApp "permutation" [xs, ys] = "isPermutation " ++ xs ++ " " ++ ys
translateApp "elem"        [x, xs]  = "Set_mem " ++ x ++ " (elems " ++ xs ++ ")"
translateApp "subset"      [xs, ys] = "Set_sub (elems " ++ xs ++ ") (elems " ++ ys ++ ")"
translateApp "negate"      [x]      = "(0 - " ++ x ++ ")"
translateApp "all"         [_, _]   = "True {- all: not yet supported -}"
translateApp name          args     = name ++ " " ++ unwords args

genBinOp :: BinOp -> String
genBinOp BEq  = "=="
genBinOp BNeq = "/="
genBinOp BLe  = "<="
genBinOp BLt  = "<"
genBinOp BGe  = ">="
genBinOp BGt  = ">"
genBinOp BAnd = "&&"
genBinOp BOr  = "||"
genBinOp BAdd = "+"
genBinOp BSub = "-"
genBinOp BMul = "*"
