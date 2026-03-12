module GuardedGen.Loop
  ( VerifiedCode(..)
  , verifyLoop
  , verifyLoopWith
  ) where

import GuardedGen.DSL.AST        (SpecDecl)
import GuardedGen.DSL.CodeGen    (genLHAnnotation)
import GuardedGen.LLM.Client     (LLMConfig, defaultConfig, callLLM, extractCode)
import GuardedGen.LLM.Prompt     (buildPrompt, buildRetryPrompt)
import GuardedGen.Verify.Runner  (VerifyResult(..), runLiquid)

-- ============================================================
-- 検証済みコードの型
-- ============================================================

data VerifiedCode = VerifiedCode
  { verifiedSource  :: String  -- 検証を通ったHaskellコード
  , verifiedSpec    :: String  -- 元のLH注釈
  , verifiedRetries :: Int     -- 何回目で成功したか
  } deriving (Show)

-- ============================================================
-- 検証ループ
-- ============================================================

-- | デフォルト設定でループを実行（最大5回リトライ）
verifyLoop :: SpecDecl -> IO (Either String VerifiedCode)
verifyLoop spec = verifyLoopWith defaultConfig 5 spec

-- | 設定とリトライ数を指定してループを実行
verifyLoopWith :: LLMConfig -> Int -> SpecDecl -> IO (Either String VerifiedCode)
verifyLoopWith cfg maxRetries spec = do
  let lhAnnotation = genLHAnnotation spec
  putStrLn $ "[GuardedGen] 仕様: " ++ show lhAnnotation
  go lhAnnotation Nothing maxRetries 0
  where
    go lhAnnotation prevError remaining attempt = do
      -- 1. プロンプト構築
      let prompt = case prevError of
            Nothing       -> buildPrompt lhAnnotation
            Just (code, err) -> buildRetryPrompt lhAnnotation code err

      putStrLn $ "[GuardedGen] LLM呼び出し中... (試行 " ++ show (attempt + 1) ++ ")"

      -- 2. LLM に実装を生成させる
      llmResult <- callLLM cfg prompt
      case llmResult of
        Left err -> return $ Left $ "LLM呼び出しエラー: " ++ err
        Right response -> do
          let code = extractCode response
          putStrLn $ "[GuardedGen] 生成されたコード:\n" ++ code

          -- 3. Liquid Haskell で検証
          putStrLn "[GuardedGen] Liquid Haskell で検証中..."
          verifyResult <- runLiquid code

          case verifyResult of
            -- 4a. 検証通過 → 成功
            Safe -> do
              putStrLn "[GuardedGen] ✓ SAFE: 検証通過！"
              return $ Right $ VerifiedCode
                { verifiedSource  = code
                , verifiedSpec    = lhAnnotation
                , verifiedRetries = attempt
                }

            -- 4b. 検証失敗 → リトライ
            Unsafe errMsg -> do
              putStrLn $ "[GuardedGen] ✗ UNSAFE: 検証失敗\n" ++ errMsg
              if remaining <= 0
                then return $ Left $
                  "最大リトライ回数 (" ++ show maxRetries ++ ") に到達しました\n"
                  ++ "最後のエラー:\n" ++ errMsg
                else go lhAnnotation (Just (code, errMsg)) (remaining - 1) (attempt + 1)

            -- 4c. liquid コマンド自体のエラー
            VerifyError msg -> do
              putStrLn $ "[GuardedGen] ! 検証エラー: " ++ msg
              if remaining <= 0
                then return $ Left $ "検証エラー: " ++ msg
                else go lhAnnotation (Just (code, msg)) (remaining - 1) (attempt + 1)
