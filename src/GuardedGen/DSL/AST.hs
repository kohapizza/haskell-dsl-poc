module GuardedGen.DSL.AST where

-- ============================================================
-- GuardedGen DSL の AST 定義
-- ============================================================

-- | トップレベルの仕様宣言
data SpecDecl = SpecDecl
  { specName    :: String    -- 関数名
  , specTypeSig :: TypeSig   -- 型シグネチャ
  , specClauses :: [Clause]  -- requires/ensures 節
  } deriving (Show, Eq)

-- | 型シグネチャ: 引数の型リスト と 戻り値の型
data TypeSig = TypeSig
  { sigArgs   :: [Type]  -- 引数型（空なら定数）
  , sigResult :: Type    -- 戻り値型
  } deriving (Show, Eq)

-- | requires/ensures 節
data Clause
  = Ensures  Expr  -- 事後条件
  | Requires Expr  -- 事前条件
  deriving (Show, Eq)

-- | 型
data Type
  = TInt               -- Int
  | TBool              -- Bool
  | TList Type         -- [T]
  | TFun Type Type     -- T -> T  (高階関数用)
  | TRefined           -- {x : T | P}
      String           --   バインド変数名
      Type             --   基底型
      Expr             --   述語
  deriving (Show, Eq)

-- | 式（述語・制約の記述に使う）
data Expr
  = EVar  String           -- 変数・キーワード (input, output, lo, hi, ...)
  | ELit  Int              -- 整数リテラル
  | EApp  String [Expr]    -- 関数適用: f(e1, e2, ...)
  | EBinOp BinOp Expr Expr -- 二項演算
  | ENeg  Expr             -- 算術否定: negate(x) の代わりに内部で使う
  deriving (Show, Eq)

-- | 二項演算子
data BinOp
  = BEq   -- ==
  | BNeq  -- /=
  | BLe   -- <=
  | BLt   -- <
  | BGe   -- >=
  | BGt   -- >
  | BAnd  -- &&
  | BOr   -- ||
  | BAdd  -- +
  | BSub  -- -
  | BMul  -- *
  deriving (Show, Eq)
