# GuardedGen

**仕様DSL × Liquid Haskell × LLM による検証付きコード生成**

人間が仕様を書く。AIが実装する。数学が正しさを保証する。

## 概要

GuardedGenは、LLMを「候補生成器」として扱い、正しさの判断をLiquid Haskell（SMTソルバーZ3）による機械検証に委ねるコード生成基盤です。

```
[人間] 仕様DSLで仕様を書く
  ↓
[DSLコンパイラ] Liquid Haskellのrefinement type注釈に変換
  ↓
[LLM] 注釈付き関数シグネチャを見て実装コードを生成
  ↓
[Liquid Haskell + Z3] 生成コードが仕様を満たすか自動検証
  ↓
  ├─ SAFE → 採用（仕様を満たすことが証明済み）
  └─ UNSAFE → エラー情報をLLMに返して再生成
               ↑___________________________|
```

## 動作環境

- GHC 9.10.1
- cabal 3.12+
- [liquidhaskell 0.9.10.1.2](https://hackage.haskell.org/package/liquidhaskell)
- z3（`brew install z3` / `apt install z3`）
- Anthropic API キー

## セットアップ

```bash
# z3のインストール（macOS）
brew install z3

# liquidhaskellのインストール
cabal install liquidhaskell

# ビルド
cabal build

# APIキーの設定
export ANTHROPIC_API_KEY=sk-ant-...
```

## 使い方

```bash
cabal run guardedgen -- run specs/clamp.spec
```

仕様ファイルを渡すと、パース → LH注釈生成 → LLM実装生成 → 検証 → リトライのパイプラインが自動で走ります。

## 仕様DSLの記法

```
spec clamp : Int -> Int -> Int -> Int
  requires lo <= hi
  ensures output >= lo
  ensures output <= hi
```

- `requires` : 事前条件（引数に対する制約）
- `ensures`  : 事後条件（戻り値に対する制約）
- `input` / `output` : 入力・出力を指すキーワード
- コメントは `#` で書く

### 対応している述語

| DSL | 意味 |
|-----|------|
| `len(xs)` | リストの長さ |
| `sorted(xs)` | ソート済みであること |
| `permutation(xs, ys)` | xs が ys の置換であること |
| `elem(x, xs)` | x が xs に含まれること |

## サンプル仕様ファイル

```
specs/
├── abs.spec     -- 絶対値（出力 >= 0）
├── clamp.spec   -- 区間クランプ（lo <= output <= hi）
├── filter.spec  -- フィルタ（出力の長さ <= 入力の長さ）
└── sort.spec    -- ソート（ソート済み + 置換 + 長さ保存）
```

## PoCの結果

| 仕様ファイル | 試行回数 | 結果 |
|---|---|---|
| abs.spec | 1回 | SAFE |
| clamp.spec | 1回 | SAFE |
| filter.spec | 1回 | SAFE |
| sort.spec | 6回 | SAFE（仕様弱体化※） |

※ `isSorted` / `isPermutation` はLiquid Haskell 0.9.10.1.2のmeasure制約（Bool値多パターン関数の持ち上げ不可）により完全な検証には至らず、LLMが長さ保存のみの仕様に収束した。

## プロジェクト構成

```
src/GuardedGen/
├── DSL/
│   ├── AST.hs       -- DSLの内部表現
│   ├── Parser.hs    -- 仕様ファイルパーサー（megaparsec）
│   └── CodeGen.hs   -- AST → Liquid Haskell注釈
├── LLM/
│   ├── Client.hs    -- Anthropic API呼び出し
│   └── Prompt.hs    -- プロンプト構築
├── Verify/
│   ├── Runner.hs    -- cabal buildでLH検証を実行
│   └── Parser.hs    -- SAFE/UNSAFE結果のパース
├── Loop.hs          -- 検証ループ（最大5リトライ）
└── Main.hs          -- CLIエントリーポイント
```
