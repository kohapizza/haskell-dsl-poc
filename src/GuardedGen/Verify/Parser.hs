module GuardedGen.Verify.Parser
  ( VerifyResultRaw(..)
  , parseVerifyResult
  , isSafe
  , extractErrors
  ) where

import Data.List (isInfixOf)

-- ============================================================
-- liquid コマンド出力のパース
-- （Runner.hs と循環しないよう独立した型を定義）
-- ============================================================

data VerifyResultRaw
  = RSafe
  | RUnsafe String   -- エラーメッセージ
  deriving (Show, Eq)

-- | liquid の出力から結果を判定
parseVerifyResult :: String -> Maybe VerifyResultRaw
parseVerifyResult output
  | "LIQUID: SAFE"   `isInfixOf` output = Just RSafe
  | "LIQUID: UNSAFE" `isInfixOf` output = Just (RUnsafe (extractErrors output))
  | otherwise                            = Nothing

-- | SAFE かどうか
isSafe :: String -> Bool
isSafe output = "LIQUID: SAFE" `isInfixOf` output

-- | エラーメッセージを抽出・整形
extractErrors :: String -> String
extractErrors output =
  unlines $ extractErrorSections (lines output)

-- | "Liquid Type Mismatch" や "error:" を含む行から取り出す
extractErrorSections :: [String] -> [String]
extractErrorSections [] = []
extractErrorSections (l:ls)
  | "Liquid Type Mismatch" `isInfixOf` l = l : takeSection ls ++ [""] ++ extractErrorSections (dropSection ls)
  | "error:"               `isInfixOf` l = l : takeSection ls ++ [""] ++ extractErrorSections (dropSection ls)
  | otherwise                            = extractErrorSections ls
  where
    takeSection = takeWhile (not . null)
    dropSection = drop 1 . dropWhile (not . null)
