module GuardedGen.LLM.Prompt
  ( buildPrompt
  , buildRetryPrompt
  ) where

-- ============================================================
-- プロンプト構築モジュール
-- ============================================================

-- | 初回生成プロンプト
-- LH注釈付きシグネチャ → 実装を求めるプロンプト
buildPrompt :: String -> String
buildPrompt lhAnnotation = unlines
  [ "以下のLiquid Haskell注釈付き関数シグネチャに対して、仕様を満たすHaskellの実装を書いてください。"
  , ""
  , "## 要件"
  , "- Liquid Haskellの検証（`liquid` コマンド）を通過する実装を書いてください"
  , "- `{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}` プラグマはすでに含まれています"
  , "- 必要な補助関数があれば定義してください"
  , "- `undefined` や `error` を使わず、完全な実装を書いてください"
  , ""
  , "## シグネチャ"
  , "```haskell"
  , lhAnnotation
  , "```"
  , ""
  , "## 出力形式"
  , "Haskellのコードブロックのみを返してください。説明は不要です。"
  , "```haskell"
  , "-- 実装をここに書く"
  , "```"
  ]

-- | リトライ時プロンプト（エラーフィードバック付き）
buildRetryPrompt :: String -> String -> String -> String
buildRetryPrompt lhAnnotation prevCode errorMsg = unlines
  [ "前回の実装がLiquid Haskellの検証を通過しませんでした。"
  , "エラーを修正した実装を書いてください。"
  , ""
  , "## シグネチャ"
  , "```haskell"
  , lhAnnotation
  , "```"
  , ""
  , "## 前回の実装"
  , "```haskell"
  , prevCode
  , "```"
  , ""
  , "## Liquid Haskellのエラー"
  , "```"
  , errorMsg
  , "```"
  , ""
  , "## エラーの読み方"
  , "- `The inferred type ... is not a subtype of the required type` は、"
  , "  実装が返す値がrefinement typeの条件を満たさないことを意味します"
  , "- `Liquid Type Mismatch` はSMTソルバーが証明できなかった箇所です"
  , "- エラー箇所の行番号を確認して、その部分の実装を修正してください"
  , ""
  , "## 出力形式"
  , "Haskellのコードブロックのみを返してください。"
  , "```haskell"
  , "-- 修正した実装をここに書く"
  , "```"
  ]
