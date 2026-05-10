module Main where

import System.Environment        (getArgs)

import GuardedGen.DSL.Parser     (parseSpec)
import GuardedGen.DSL.CodeGen    (genLHAnnotation)
import GuardedGen.Loop           (verifyLoop)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- `guardedgen run <spec-file>` でループ実行
    ["run", specFile] -> runFromFile specFile

    -- 引数なし: DSL変換デモ
    _ -> runDemoConversion

-- ============================================================
-- DSL変換デモ（LLM不要）
-- ============================================================

runDemoConversion :: IO ()
runDemoConversion = mapM_ runExample examples
  where
    examples =
      [ ("myAbs",
         "spec myAbs : Int -> Int\n\
         \  ensures output >= 0\n\
         \  ensures output == input || output == negate(input)\n")
      , ("clamp",
         "spec clamp : Int -> Int -> Int -> Int\n\
         \  requires lo <= hi\n\
         \  ensures output >= lo\n\
         \  ensures output <= hi\n")
      , ("myFilter",
         "spec myFilter : (Int -> Bool) -> [Int] -> [Int]\n\
         \  ensures len(output) <= len(input)\n")
      , ("sort",
         "spec sort : [Int] -> [Int]\n\
         \  ensures sorted(output)\n\
         \  ensures permutation(output, input)\n\
         \  ensures len(output) == len(input)\n")
      ]

    runExample (name, src) = do
      putStrLn $ "=== " ++ name ++ " ==="
      putStrLn $ "Input DSL:\n" ++ src
      case parseSpec name src of
        Left err  -> putStrLn $ "Parse error: " ++ err
        Right ast -> do
          putStrLn "Generated LH annotation:"
          putStrLn $ genLHAnnotation ast

-- ============================================================
-- ファイルから読み込んでループ実行
-- ============================================================

runFromFile :: FilePath -> IO ()
runFromFile specFile = do
  src <- readFile specFile
  case parseSpec specFile src of
    Left err  -> putStrLn $ "Parse error: " ++ err
    Right ast -> do
      result <- verifyLoop ast
      case result of
        Left  err  -> putStrLn $ "失敗: " ++ err
        Right code -> do
          putStrLn "=== 検証済みコード ==="
          putStrLn $ show code
