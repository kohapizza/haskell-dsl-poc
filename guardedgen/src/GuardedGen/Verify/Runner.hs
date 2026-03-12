module GuardedGen.Verify.Runner
  ( VerifyResult(..)
  , runLiquid
  ) where

import System.Process    (readCreateProcessWithExitCode, proc, CreateProcess(..), StdStream(..))
import System.Exit       (ExitCode(..))
import System.IO.Temp    (withSystemTempDirectory)
import System.IO         (writeFile, hGetContents)
import System.FilePath   ((</>))
import System.Directory  (createDirectory)

import GuardedGen.Verify.Parser (parseVerifyResult, VerifyResultRaw(..))

-- ============================================================
-- 検証結果の型
-- ============================================================

data VerifyResult
  = Safe                -- 検証通過
  | Unsafe String       -- 検証失敗（エラーメッセージ付き）
  | VerifyError String  -- ビルド自体のエラー
  deriving (Show, Eq)

-- ============================================================
-- Liquid Haskell の実行（cabal build プラグイン方式）
-- ============================================================

runLiquid :: String -> IO VerifyResult
runLiquid code =
  withSystemTempDirectory "guardedgen-verify" $ \tmpDir -> do
    let srcDir    = tmpDir </> "src"
    let srcFile   = srcDir </> "Tmp.hs"
    let cabalFile = tmpDir </> "verify.cabal"
    let projFile  = tmpDir </> "cabal.project"

    createDirectory srcDir
    System.IO.writeFile srcFile   (wrapCode code)
    System.IO.writeFile cabalFile cabalContent
    System.IO.writeFile projFile  "packages: .\n"

    let cp = (proc "cabal" ["build"])
               { cwd = Just tmpDir }
    (exitCode, stdout, stderr) <-
      readCreateProcessWithExitCode cp ""

    let output = stdout ++ stderr
    case exitCode of
      ExitSuccess   -> return Safe
      ExitFailure _ ->
        case parseVerifyResult output of
          Just RSafe       -> return Safe
          Just (RUnsafe e) -> return (Unsafe e)
          Nothing          -> return (VerifyError output)

-- | コードに LH プラグマと llen measure のみを付加
-- isSorted/isPermutation は LLM に定義させる（measure の制約が複雑なため）
wrapCode :: String -> String
wrapCode code = unlines
  [ "{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}"
  , "module Tmp where"
  , ""
  , "{-@ measure llen @-}"
  , "llen :: [a] -> Int"
  , "llen []     = 0"
  , "llen (_:xs) = 1 + llen xs"
  , "{-@ invariant {v:[a] | llen v >= 0} @-}"
  , ""
  , stripBoilerplate code
  ]

-- | LLM 生成コードから既に wrapCode に含まれる定義を除去
-- 除去対象: module宣言, OPTIONS, LANGUAGE, llen定義, isSorted定義,
--           count定義, isPermutation定義, invariant
stripBoilerplate :: String -> String
stripBoilerplate code =
  let ls      = lines code
      cleaned = dropDefs ls
  in unlines cleaned

-- | 既知の定義ブロックを除去（シンプルな行単位フィルタ）
dropDefs :: [String] -> [String]
dropDefs [] = []
dropDefs (l:ls)
  -- ヘッダー行を除去
  | any (`isPrefixOf` l)
      [ "module ", "{-# OPTIONS", "{-# LANGUAGE"
      , "{-@ measure llen", "llen ::", "llen []", "llen (_"
      , "{-@ measure isSorted", "isSorted ::", "isSorted []", "isSorted [_]", "isSorted (x:y"
      , "{-@ measure count", "count ::", "count _", "count n ("
      , "{-@ inline isPermutation", "isPermutation ::", "isPermutation xs"
      , "{-@ invariant"
      , "{-@ llen ::"
      ] = dropDefs ls
  | otherwise = l : dropDefs ls

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

-- | 一時プロジェクトの .cabal（相対 hs-source-dirs）
cabalContent :: String
cabalContent = unlines
  [ "cabal-version: 3.0"
  , "name:          guardedgen-verify"
  , "version:       0.1.0.0"
  , "build-type:    Simple"
  , ""
  , "library"
  , "  hs-source-dirs:   src"
  , "  default-language: Haskell2010"
  , "  exposed-modules:  Tmp"
  , "  build-depends:"
  , "    base          >= 4.14 && < 5,"
  , "    liquidhaskell == 0.9.10.1.2"
  , "  ghc-options: -fplugin=LiquidHaskell"
  ]
