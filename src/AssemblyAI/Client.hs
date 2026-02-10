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
  , acApiKey  :: ApiKey
  }

-- | Create a new AssemblyAI client
mkAssemblyAIClient :: ApiKey -> IO AssemblyAIClient
mkAssemblyAIClient apiKey = do
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "https://api.assemblyai.com"
  pure $ AssemblyAIClient manager baseUrl apiKey

-- | Run an AssemblyAI API call
runAssemblyAI :: AssemblyAIClient -> ClientM a -> IO (Either Text a)
runAssemblyAI env action = do
  let clientEnv = mkClientEnv (acManager env) (acBaseUrl env)
  result <- runClientM action clientEnv
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right r  -> pure $ Right r

-- | Create a new transcript using the client's API key
createTranscript :: AssemblyAIClient -> TranscriptRequest -> ClientM Transcript
createTranscript env req = 
  let ApiKey key = acApiKey env
  in createTranscript' key req

-- | Get a transcript by ID using the client's API key
getTranscript :: AssemblyAIClient -> TranscriptId -> ClientM Transcript
getTranscript env tid = 
  let ApiKey key = acApiKey env
  in getTranscript' key tid

-- Internal client functions
createTranscript' :: Text -> TranscriptRequest -> ClientM Transcript
getTranscript' :: Text -> TranscriptId -> ClientM Transcript
createTranscript' :<|> getTranscript' = client assemblyAIAPI
