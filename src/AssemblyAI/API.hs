{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AssemblyAI.API
  ( AssemblyAIAPI
  , TranscriptAPI
  , UploadAPI
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
  , UploadedFile
  , WordSearchResponse
  )
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API
  ( Capture
  , Delete
  , Get
  , Header'
  , JSON
  , OctetStream
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

-- | The full AssemblyAI API.
--
-- Authentication is factored out so it appears only once.  The @\/v2@
-- prefix is shared and then the API branches into transcript and upload
-- sub-APIs.
type AssemblyAIAPI =
  Header' '[Required, Strict] "authorization" Text
    :> "v2"
    :> (    "transcript" :> TranscriptAPI
       :<|> "upload" :> UploadAPI
       )

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

-- | File upload endpoint.
--
-- @POST \/v2\/upload@ — upload a media file as raw binary data.
type UploadAPI =
  ReqBody '[OctetStream] ByteString :> Post '[JSON] UploadedFile

-- | Proxy for the AssemblyAI API
assemblyAIAPI :: Proxy AssemblyAIAPI
assemblyAIAPI = Proxy
