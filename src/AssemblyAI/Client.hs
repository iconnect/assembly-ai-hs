
{-# LANGUAGE TypeOperators #-}

module AssemblyAI.Client
  ( -- * Client Environment
    AssemblyAIClient
  , mkAssemblyAIClient
  , runAssemblyAI
    -- * Transcript Operations
  , createTranscript
  , listTranscripts
  , getTranscript
  , deleteTranscript
    -- * Transcript Content
  , getSentences
  , getParagraphs
  , getSubtitles
    -- * Transcript Analysis
  , searchWords
  , getRedactedAudio
  ) where

import AssemblyAI.API (assemblyAIAPI)
import AssemblyAI.Types
  ( ApiKey (..)
  , ParagraphsResponse
  , RedactedAudioResponse
  , SentencesResponse
  , SubtitleFormat
  , Transcript
  , TranscriptId
  , TranscriptList
  , TranscriptRequest
  , TranscriptStatus
  , WordSearchResponse
  )
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (type (:<|>) (..))
import Servant.Client (BaseUrl, ClientM, client, mkClientEnv, runClientM)

-- | Client environment for AssemblyAI API
data AssemblyAIClient = AssemblyAIClient
  { acManager :: Manager
  , acBaseUrl :: BaseUrl
  , acApiKey  :: ApiKey
  }

-- | Create a new AssemblyAI client. The 'BaseUrl' is passed
-- as a parameter because depending on the region the user might
-- want to use a different endpoint (like the EU one, for example).
mkAssemblyAIClient :: BaseUrl -> ApiKey -> IO AssemblyAIClient
mkAssemblyAIClient baseUrl apiKey = do
  manager <- newManager tlsManagerSettings
  pure $ AssemblyAIClient manager baseUrl apiKey

-- | Run an AssemblyAI API call
runAssemblyAI :: AssemblyAIClient -> ClientM a -> IO (Either Text a)
runAssemblyAI env action = do
  let clientEnv = mkClientEnv (acManager env) (acBaseUrl env)
  result <- runClientM action clientEnv
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right r  -> pure $ Right r

-- | Extract the raw key text from a client
apiKeyText :: AssemblyAIClient -> Text
apiKeyText env = let ApiKey k = acApiKey env in k

-- | Create a new transcript
createTranscript :: AssemblyAIClient -> TranscriptRequest -> ClientM Transcript
createTranscript env = createTranscript' (apiKeyText env)

-- | List transcripts with optional filters
listTranscripts
  :: AssemblyAIClient
  -> Maybe Int               -- ^ Maximum number of transcripts to retrieve
  -> Maybe TranscriptStatus  -- ^ Filter by status
  -> Maybe Text              -- ^ Only get transcripts created on this date
  -> Maybe Text              -- ^ Get transcripts created before this transcript ID
  -> Maybe Text              -- ^ Get transcripts created after this transcript ID
  -> Maybe Bool              -- ^ Only get throttled transcripts
  -> ClientM TranscriptList
listTranscripts env = listTranscripts' (apiKeyText env)

-- | Get a transcript by ID
getTranscript :: AssemblyAIClient -> TranscriptId -> ClientM Transcript
getTranscript env = getTranscript' (apiKeyText env)

-- | Delete a transcript by ID
deleteTranscript :: AssemblyAIClient -> TranscriptId -> ClientM Transcript
deleteTranscript env = deleteTranscript' (apiKeyText env)

-- | Get sentences for a transcript
getSentences :: AssemblyAIClient -> TranscriptId -> ClientM SentencesResponse
getSentences env = getSentences' (apiKeyText env)

-- | Get paragraphs for a transcript
getParagraphs :: AssemblyAIClient -> TranscriptId -> ClientM ParagraphsResponse
getParagraphs env = getParagraphs' (apiKeyText env)

-- | Get subtitles for a transcript in SRT or VTT format
getSubtitles
  :: AssemblyAIClient
  -> TranscriptId
  -> SubtitleFormat
  -> Maybe Int       -- ^ Maximum characters per caption
  -> ClientM Text
getSubtitles env = getSubtitles' (apiKeyText env)

-- | Search for words in a transcript.
-- The words parameter should be a comma-separated list of keywords.
searchWords :: AssemblyAIClient -> TranscriptId -> Text -> ClientM WordSearchResponse
searchWords env = searchWords' (apiKeyText env)

-- | Get the redacted audio for a transcript
getRedactedAudio :: AssemblyAIClient -> TranscriptId -> ClientM RedactedAudioResponse
getRedactedAudio env = getRedactedAudio' (apiKeyText env)

-- Internal: the factored API means `client assemblyAIAPI` produces a single
-- function @Text -> (ep1 :<|> ep2 :<|> ... :<|> ep9)@.  We apply the auth
-- key once to get the endpoint tree, then destructure it.
endpoints
  :: Text
  -> (TranscriptRequest -> ClientM Transcript)
     :<|> (Maybe Int -> Maybe TranscriptStatus -> Maybe Text
           -> Maybe Text -> Maybe Text -> Maybe Bool -> ClientM TranscriptList)
     :<|> (TranscriptId -> ClientM Transcript)
     :<|> (TranscriptId -> ClientM Transcript)
     :<|> (TranscriptId -> ClientM SentencesResponse)
     :<|> (TranscriptId -> ClientM ParagraphsResponse)
     :<|> (TranscriptId -> SubtitleFormat -> Maybe Int -> ClientM Text)
     :<|> (TranscriptId -> Text -> ClientM WordSearchResponse)
     :<|> (TranscriptId -> ClientM RedactedAudioResponse)
endpoints = client assemblyAIAPI

createTranscript' :: Text -> TranscriptRequest -> ClientM Transcript
createTranscript' key req =
  let f :<|> _ = endpoints key in f req

listTranscripts'
  :: Text -> Maybe Int -> Maybe TranscriptStatus -> Maybe Text
  -> Maybe Text -> Maybe Text -> Maybe Bool -> ClientM TranscriptList
listTranscripts' key a b c d e f =
  let _ :<|> g :<|> _ = endpoints key in g a b c d e f

getTranscript' :: Text -> TranscriptId -> ClientM Transcript
getTranscript' key tid =
  let _ :<|> _ :<|> f :<|> _ = endpoints key in f tid

deleteTranscript' :: Text -> TranscriptId -> ClientM Transcript
deleteTranscript' key tid =
  let _ :<|> _ :<|> _ :<|> f :<|> _ = endpoints key in f tid

getSentences' :: Text -> TranscriptId -> ClientM SentencesResponse
getSentences' key tid =
  let _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ = endpoints key in f tid

getParagraphs' :: Text -> TranscriptId -> ClientM ParagraphsResponse
getParagraphs' key tid =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ = endpoints key in f tid

getSubtitles' :: Text -> TranscriptId -> SubtitleFormat -> Maybe Int -> ClientM Text
getSubtitles' key tid fmt mChars =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ = endpoints key in f tid fmt mChars

searchWords' :: Text -> TranscriptId -> Text -> ClientM WordSearchResponse
searchWords' key tid ws =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ = endpoints key in f tid ws

getRedactedAudio' :: Text -> TranscriptId -> ClientM RedactedAudioResponse
getRedactedAudio' key tid =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f = endpoints key in f tid
