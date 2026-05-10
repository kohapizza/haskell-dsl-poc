{-# LANGUAGE OverloadedStrings #-}
module GuardedGen.LLM.Client
  ( LLMConfig(..)
  , defaultConfig
  , callLLM
  , extractCode
  ) where

import           Control.Exception          (SomeException, try)
import           Data.Aeson                 (object, (.=), encode, (.:))
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as AT
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Environment         (lookupEnv)

-- ============================================================
-- 設定
-- ============================================================

data LLMConfig = LLMConfig
  { llmModel     :: String
  , llmMaxTokens :: Int
  , llmApiKeyEnv :: String
  } deriving (Show)

defaultConfig :: LLMConfig
defaultConfig = LLMConfig
  { llmModel     = "claude-opus-4-6"
  , llmMaxTokens = 2048
  , llmApiKeyEnv = "ANTHROPIC_API_KEY"
  }

-- ============================================================
-- API 呼び出し
-- ============================================================

callLLM :: LLMConfig -> String -> IO (Either String String)
callLLM cfg prompt = do
  apiKey <- lookupEnv (llmApiKeyEnv cfg)
  case apiKey of
    Nothing  -> return $ Left $ "環境変数 " ++ llmApiKeyEnv cfg ++ " が未設定です"
    Just key -> do
      result <- try (doRequest cfg key prompt) :: IO (Either SomeException String)
      return $ case result of
        Left  ex  -> Left (show ex)
        Right txt -> Right txt

doRequest :: LLMConfig -> String -> String -> IO String
doRequest cfg apiKey prompt = do
  manager <- newManager tlsManagerSettings

  let reqBody = encode $ object
        [ "model"      .= llmModel cfg
        , "max_tokens" .= llmMaxTokens cfg
        , "messages"   .= [ object
              [ "role"    .= ("user" :: String)
              , "content" .= prompt
              ] ]
        ]

  initReq <- parseRequest "POST https://api.anthropic.com/v1/messages"
  let req = initReq
        { method         = "POST"
        , requestHeaders =
            [ ("x-api-key",         BS.pack apiKey)
            , ("anthropic-version", "2023-06-01")
            , ("content-type",      "application/json")
            ]
        , requestBody    = RequestBodyLBS reqBody
        }

  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  let body   = responseBody resp

  if status /= 200
    then error $ "API error " ++ show status ++ ": " ++ LBS.unpack body
    else parseResponse body

-- ============================================================
-- レスポンスのパース（aeson 2.x 対応）
-- ============================================================

parseResponse :: ByteString -> IO String
parseResponse body =
  case Aeson.eitherDecode body :: Either String Aeson.Value of
    Left  err -> error $ "JSON decode error: " ++ err
    Right val ->
      case AT.parseEither parseAnthropicContent val of
        Left  err -> error $ "Response parse error: " ++ err ++ "\nBody: " ++ LBS.unpack body
        Right txt -> return txt

-- Anthropic レスポンス:
-- { "content": [ { "type": "text", "text": "..." } ] }
parseAnthropicContent :: Aeson.Value -> AT.Parser String
parseAnthropicContent = Aeson.withObject "response" $ \o -> do
  contents <- o .: "content"
  case contents of
    Aeson.Array arr ->
      case V.toList arr of
        (first:_) -> extractText first
        []        -> fail "empty content array"
    _ -> fail "content is not an array"
  where
    extractText = Aeson.withObject "content block" $ \o -> do
      typ <- o .: "type" :: AT.Parser String
      case typ of
        "text" -> T.unpack <$> (o .: "text")
        other  -> fail $ "unexpected type: " ++ other

-- ============================================================
-- コード抽出
-- ============================================================

extractCode :: String -> String
extractCode response =
  case extractBlock "```haskell" response of
    Just code -> code
    Nothing   ->
      case extractBlock "```" response of
        Just code -> code
        Nothing   -> response

extractBlock :: String -> String -> Maybe String
extractBlock fence text =
  let ls = lines text
  in case dropWhile (/= fence) ls of
    []       -> Nothing
    (_:rest) ->
      let inner = takeWhile (\l -> l /= "```" && l /= fence) rest
      in Just (unlines inner)
