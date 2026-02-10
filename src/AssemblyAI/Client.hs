{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module AssemblyAI.Client
  ( -- * Client Functions
    createTranscript
  , getTranscript
    -- * Client Environment
  , AssemblyAIClient
  , mkAssemblyAIClient
  , runAssemblyAI
  ) where

import AssemblyAI.API (assemblyAIAPI)
import AssemblyAI.Types (ApiKey (..), Transcript, TranscriptId, TranscriptRequest)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (type (:<|>) (..))
import Servant.Client (BaseUrl, ClientM, client, mkClientEnv, parseBaseUrl, runClientM)

-- | Client environment for AssemblyAI API
data AssemblyAIClient = AssemblyAIClient
  { acManager :: Manager
  , acBaseUrl :: BaseUrl
  }

-- | Create a new AssemblyAI client
mkAssemblyAIClient :: ApiKey -> IO AssemblyAIClient
mkAssemblyAIClient _ = do
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "https://api.assemblyai.com"
  pure $ AssemblyAIClient manager baseUrl

-- | Run an AssemblyAI API call
runAssemblyAI :: AssemblyAIClient -> ClientM a -> IO (Either Text a)
runAssemblyAI env action = do
  let clientEnv = mkClientEnv (acManager env) (acBaseUrl env)
  result <- runClientM action clientEnv
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right r  -> pure $ Right r

-- | Create a new transcript
createTranscript :: ApiKey -> TranscriptRequest -> ClientM Transcript
createTranscript (ApiKey key) req = createTranscript' key req

-- | Get a transcript by ID
getTranscript :: ApiKey -> TranscriptId -> ClientM Transcript
getTranscript (ApiKey key) tid = getTranscript' key tid

-- Internal client functions
createTranscript' :: Text -> TranscriptRequest -> ClientM Transcript
getTranscript' :: Text -> TranscriptId -> ClientM Transcript
createTranscript' :<|> getTranscript' = client assemblyAIAPI
