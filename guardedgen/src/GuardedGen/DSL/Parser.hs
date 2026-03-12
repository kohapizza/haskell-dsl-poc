module GuardedGen.DSL.Parser
  ( parseSpec
  , parseSpecs
  ) where

import           Control.Monad              (void)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import GuardedGen.DSL.AST

-- ============================================================
-- パーサーの型
-- ============================================================

type Parser = Parsec Void String

-- ============================================================
-- レキサー（空白・コメント処理）
-- ============================================================

-- 行コメント (#) のみサポート
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- インライン空白（改行を含まない）
scInline :: Parser ()
scInline = L.space (void $ takeWhile1P Nothing (\c -> c == ' ' || c == '\t'))
                   lineComment
                   empty

-- 空白（改行を含む）
sc :: Parser ()
sc = L.space space1 lineComment empty

-- レキサーヘルパー
lexeme :: Parser a -> Parser a
lexeme = L.lexeme scInline

symbol :: String -> Parser String
symbol = L.symbol scInline

-- ============================================================
-- 基本トークン
-- ============================================================

-- 識別子（変数名・関数名）
identifier :: Parser String
identifier = lexeme $ do
  c  <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  return (c:cs)

-- キーワード（identifierだが特定の単語）
keyword :: String -> Parser ()
keyword w = void $ lexeme $ string w <* notFollowedBy (alphaNumChar <|> char '_')

-- 整数リテラル
integer :: Parser Int
integer = lexeme L.decimal

-- ============================================================
-- 型パーサー
-- ============================================================

-- 型シグネチャ全体: T1 -> T2 -> ... -> Tn
parseTypeSig :: Parser TypeSig
parseTypeSig = do
  types <- parseType `sepBy1` (symbol "->")
  case types of
    []  -> fail "empty type signature"
    [t] -> return $ TypeSig [] t
    _   -> return $ TypeSig (init types) (last types)

-- 単一の型
parseType :: Parser Type
parseType = try parseRefinedType
        <|> try parseFunType
        <|> parseBaseType

-- 基底型（括弧あり・なし）
parseBaseType :: Parser Type
parseBaseType
  =   (keyword "Int"  >> return TInt)
  <|> (keyword "Bool" >> return TBool)
  <|> parseListType
  <|> between (symbol "(") (symbol ")") parseType

-- リスト型 [T]
parseListType :: Parser Type
parseListType = between (symbol "[") (symbol "]") $ do
  TList <$> parseBaseType

-- 括弧内の関数型 (A -> B)
parseFunType :: Parser Type
parseFunType = between (symbol "(") (symbol ")") $ do
  a <- parseBaseType
  void $ symbol "->"
  b <- parseBaseType
  return $ TFun a b

-- Refined型 {x : T | P}
parseRefinedType :: Parser Type
parseRefinedType = between (symbol "{") (symbol "}") $ do
  name <- identifier
  void $ symbol ":"
  t    <- parseBaseType
  void $ symbol "|"
  e    <- parseExpr
  return $ TRefined name t e

-- ============================================================
-- 式パーサー
-- ============================================================

parseExpr :: Parser Expr
parseExpr = parseOrExpr

-- || の左結合
parseOrExpr :: Parser Expr
parseOrExpr = chainl1 parseAndExpr (EBinOp BOr <$ symbol "||")

-- && の左結合
parseAndExpr :: Parser Expr
parseAndExpr = chainl1 parseRelExpr (EBinOp BAnd <$ symbol "&&")

-- 比較演算子
parseRelExpr :: Parser Expr
parseRelExpr = do
  l  <- parseAddExpr
  op <- optional parseRelOp
  case op of
    Nothing  -> return l
    Just bop -> EBinOp bop l <$> parseAddExpr

parseRelOp :: Parser BinOp
parseRelOp
  =   (symbol "==" >> return BEq)
  <|> (symbol "/=" >> return BNeq)
  <|> (symbol "<=" >> return BLe)
  <|> (symbol ">=" >> return BGe)
  <|> (symbol "<"  >> return BLt)
  <|> (symbol ">"  >> return BGt)

-- 加減算
parseAddExpr :: Parser Expr
parseAddExpr = chainl1 parseMulExpr addOp
  where
    addOp = (EBinOp BAdd <$ symbol "+")
        <|> (EBinOp BSub <$ symbol "-")

-- 乗算
parseMulExpr :: Parser Expr
parseMulExpr = chainl1 parseTerm (EBinOp BMul <$ symbol "*")

-- 項: 関数適用 / 変数 / リテラル / 括弧
parseTerm :: Parser Expr
parseTerm
  =   between (symbol "(") (symbol ")") parseExpr
  <|> ELit <$> integer
  <|> try parseApp
  <|> EVar <$> identifier

-- 関数適用: name(arg1, arg2, ...)
parseApp :: Parser Expr
parseApp = do
  name <- identifier
  args <- between (symbol "(") (symbol ")") $
            parseExpr `sepBy` symbol ","
  return $ EApp name args

-- chainl1 ヘルパー（megaparsecには含まれない）
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> return x

-- ============================================================
-- 節パーサー（requires / ensures）
-- ============================================================

parseClause :: Parser Clause
parseClause = do
  -- インデント（スペースまたはタブ）
  void $ some (char ' ' <|> char '\t')
  kw <- (keyword "ensures"  >> return "ensures")
    <|> (keyword "requires" >> return "requires")
  e  <- parseExpr
  case kw of
    "ensures"  -> return $ Ensures e
    "requires" -> return $ Requires e
    _          -> fail "unexpected keyword"

-- ============================================================
-- spec 宣言パーサー
-- ============================================================

parseSpecDecl :: Parser SpecDecl
parseSpecDecl = do
  sc
  keyword "spec"
  name <- identifier
  void $ symbol ":"
  typeSig <- parseTypeSig
  void eol <|> eof
  clauses <- many (try (parseClause <* (void eol <|> eof)))
  return $ SpecDecl name typeSig clauses

-- 複数のspec宣言
parseSpecs :: Parser [SpecDecl]
parseSpecs = many parseSpecDecl <* eof

-- 単一のspec宣言をパース（エラーメッセージ付き）
parseSpec :: String -> String -> Either String SpecDecl
parseSpec filename input =
  case parse (sc *> parseSpecDecl <* eof) filename input of
    Left err  -> Left (errorBundlePretty err)
    Right res -> Right res
