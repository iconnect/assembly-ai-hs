{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AssemblyAI.API
  ( AssemblyAIAPI
  , TranscriptAPI
  , assemblyAIAPI
  ) where

import AssemblyAI.Types
  ( ParagraphsResponse
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
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API
  ( Capture
  , Delete
  , Get
  , Header'
  , JSON
  , PlainText
  , Post
  , QueryParam
  , QueryParam'
  , ReqBody
  , Required
  , Strict
  , type (:<|>)
  , type (:>)
  )

-- | The AssemblyAI API for transcripts.
--
-- Authentication and the @\/v2\/transcript@ base path are factored out
-- so they appear only once.
type AssemblyAIAPI =
  Header' '[Required, Strict] "authorization" Text
    :> "v2" :> "transcript" :> TranscriptAPI

-- | All transcript endpoints, without the shared auth header and base path.
type TranscriptAPI =
  -- POST /v2/transcript — create a transcript
       ReqBody '[JSON] TranscriptRequest :> Post '[JSON] Transcript

  -- GET /v2/transcript — list transcripts
  :<|> QueryParam "limit" Int
    :> QueryParam "status" TranscriptStatus
    :> QueryParam "created_on" Text
    :> QueryParam "before_id" Text
    :> QueryParam "after_id" Text
    :> QueryParam "throttled_only" Bool
    :> Get '[JSON] TranscriptList

  -- GET /v2/transcript/:transcript_id — get a transcript
  :<|> Capture "transcript_id" TranscriptId :> Get '[JSON] Transcript

  -- DELETE /v2/transcript/:transcript_id — delete a transcript
  :<|> Capture "transcript_id" TranscriptId :> Delete '[JSON] Transcript

  -- GET /v2/transcript/:transcript_id/sentences
  :<|> Capture "transcript_id" TranscriptId
    :> "sentences" :> Get '[JSON] SentencesResponse

  -- GET /v2/transcript/:transcript_id/paragraphs
  :<|> Capture "transcript_id" TranscriptId
    :> "paragraphs" :> Get '[JSON] ParagraphsResponse

  -- GET /v2/transcript/:transcript_id/:subtitle_format
  :<|> Capture "transcript_id" TranscriptId
    :> Capture "subtitle_format" SubtitleFormat
    :> QueryParam "chars_per_caption" Int
    :> Get '[PlainText] Text

  -- GET /v2/transcript/:transcript_id/word-search?words=...
  :<|> Capture "transcript_id" TranscriptId
    :> "word-search"
    :> QueryParam' '[Required, Strict] "words" Text
    :> Get '[JSON] WordSearchResponse

  -- GET /v2/transcript/:transcript_id/redacted-audio
  :<|> Capture "transcript_id" TranscriptId
    :> "redacted-audio"
    :> Get '[JSON] RedactedAudioResponse

-- | Proxy for the AssemblyAI API
assemblyAIAPI :: Proxy AssemblyAIAPI
assemblyAIAPI = Proxy
