{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module AssemblyAI.Types
  ( -- * API Key
    ApiKey (..)
    -- * Transcript Types
  , Transcript (..)
  , TranscriptRequest (..)
  , TranscriptStatus (..)
  , TranscriptId (..)
  , AudioUrl (..)
  , SpeechModel (..)
    -- * List Transcripts
  , TranscriptList (..)
  , TranscriptListItem (..)
  , PageDetails (..)
    -- * Sentences and Paragraphs
  , SentencesResponse (..)
  , ParagraphsResponse (..)
  , TimestampedText (..)
  , WordInfo (..)
    -- * Subtitles
  , SubtitleFormat (..)
    -- * Word Search
  , WordSearchResponse (..)
  , WordSearchMatch (..)
    -- * Redacted Audio
  , RedactedAudioResponse (..)
    -- * File Upload
  , UploadedFile (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData (ToHttpApiData (..))

-- | API key for authentication
newtype ApiKey = ApiKey Text
  deriving newtype (Show, Eq)

-- | Unique identifier for a transcript
newtype TranscriptId = TranscriptId Text
  deriving stock (Show, Eq, Generic)

instance FromJSON TranscriptId
instance ToJSON TranscriptId
instance ToHttpApiData TranscriptId where
  toUrlPiece (TranscriptId tid) = tid

-- | URL pointing to an audio file
newtype AudioUrl = AudioUrl Text
  deriving stock (Show, Eq, Generic)

instance FromJSON AudioUrl
instance ToJSON AudioUrl

-- | Status of a transcript
data TranscriptStatus
  = Queued
  | Processing
  | Completed
  | Error
  deriving stock (Show, Eq, Generic)

instance FromJSON TranscriptStatus where
  parseJSON = withText "TranscriptStatus" $ \t -> case t of
    "queued"     -> pure Queued
    "processing" -> pure Processing
    "completed"  -> pure Completed
    "error"      -> pure Error
    _            -> fail $ "Unknown transcript status: " ++ show t

instance ToJSON TranscriptStatus where
  toJSON Queued     = "queued"
  toJSON Processing = "processing"
  toJSON Completed  = "completed"
  toJSON Error      = "error"

instance ToHttpApiData TranscriptStatus where
  toUrlPiece Queued     = "queued"
  toUrlPiece Processing = "processing"
  toUrlPiece Completed  = "completed"
  toUrlPiece Error      = "error"

-- | Speech model to use for transcription
data SpeechModel
  = Universal3Pro
  | Universal2
  deriving stock (Show, Eq, Generic)

instance FromJSON SpeechModel where
  parseJSON = withText "SpeechModel" $ \t -> case t of
    "universal-3-pro" -> pure Universal3Pro
    "universal-2"     -> pure Universal2
    _                 -> fail $ "Unknown speech model: " ++ show t

instance ToJSON SpeechModel where
  toJSON Universal3Pro = "universal-3-pro"
  toJSON Universal2    = "universal-2"

-- | Request to create a new transcript
data TranscriptRequest = TranscriptRequest
  { trAudioUrl      :: AudioUrl
  , trSpeechModels  :: [SpeechModel]
  } deriving stock (Show, Eq, Generic)

instance ToJSON TranscriptRequest where
  toJSON r = object
    [ "audio_url"      .= trAudioUrl r
    , "speech_models"  .= trSpeechModels r
    ]

-- | A transcript from the AssemblyAI API
data Transcript = Transcript
  { tId       :: TranscriptId
  , tStatus   :: TranscriptStatus
  , tAudioUrl :: AudioUrl
  , tText     :: Maybe Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON Transcript where
  parseJSON = withObject "Transcript" $ \o -> Transcript
    <$> o .:  "id"
    <*> o .:  "status"
    <*> o .:  "audio_url"
    <*> o .:? "text"

instance ToJSON Transcript where
  toJSON t = object
    [ "id"        .= tId t
    , "status"    .= tStatus t
    , "audio_url" .= tAudioUrl t
    , "text"      .= tText t
    ]

-- | Pagination details for transcript listing
data PageDetails = PageDetails
  { pdLimit       :: Int
  , pdResultCount :: Int
  , pdCurrentUrl  :: Text
  , pdPrevUrl     :: Maybe Text
  , pdNextUrl     :: Maybe Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON PageDetails where
  parseJSON = withObject "PageDetails" $ \o -> PageDetails
    <$> o .:  "limit"
    <*> o .:  "result_count"
    <*> o .:  "current_url"
    <*> o .:? "prev_url"
    <*> o .:? "next_url"

instance ToJSON PageDetails where
  toJSON p = object
    [ "limit"        .= pdLimit p
    , "result_count" .= pdResultCount p
    , "current_url"  .= pdCurrentUrl p
    , "prev_url"     .= pdPrevUrl p
    , "next_url"     .= pdNextUrl p
    ]

-- | Summary item in a transcript listing
data TranscriptListItem = TranscriptListItem
  { tliId          :: TranscriptId
  , tliResourceUrl :: Text
  , tliStatus      :: TranscriptStatus
  , tliCreated     :: Text
  , tliCompleted   :: Maybe Text
  , tliAudioUrl    :: AudioUrl
  , tliError       :: Maybe Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON TranscriptListItem where
  parseJSON = withObject "TranscriptListItem" $ \o -> TranscriptListItem
    <$> o .:  "id"
    <*> o .:  "resource_url"
    <*> o .:  "status"
    <*> o .:  "created"
    <*> o .:? "completed"
    <*> o .:  "audio_url"
    <*> o .:? "error"

instance ToJSON TranscriptListItem where
  toJSON t = object
    [ "id"           .= tliId t
    , "resource_url" .= tliResourceUrl t
    , "status"       .= tliStatus t
    , "created"      .= tliCreated t
    , "completed"    .= tliCompleted t
    , "audio_url"    .= tliAudioUrl t
    , "error"        .= tliError t
    ]

-- | Paginated list of transcripts
data TranscriptList = TranscriptList
  { tlPageDetails  :: PageDetails
  , tlTranscripts  :: [TranscriptListItem]
  } deriving stock (Show, Eq, Generic)

instance FromJSON TranscriptList where
  parseJSON = withObject "TranscriptList" $ \o -> TranscriptList
    <$> o .: "page_details"
    <*> o .: "transcripts"

instance ToJSON TranscriptList where
  toJSON t = object
    [ "page_details" .= tlPageDetails t
    , "transcripts"  .= tlTranscripts t
    ]

-- | Word-level information in a transcript
data WordInfo = WordInfo
  { wiText       :: Text
  , wiStart      :: Int
  , wiEnd        :: Int
  , wiConfidence :: Double
  , wiChannel    :: Maybe Text
  , wiSpeaker    :: Maybe Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON WordInfo where
  parseJSON = withObject "WordInfo" $ \o -> WordInfo
    <$> o .:  "text"
    <*> o .:  "start"
    <*> o .:  "end"
    <*> o .:  "confidence"
    <*> o .:? "channel"
    <*> o .:? "speaker"

instance ToJSON WordInfo where
  toJSON w = object
    [ "text"       .= wiText w
    , "start"      .= wiStart w
    , "end"        .= wiEnd w
    , "confidence" .= wiConfidence w
    , "channel"    .= wiChannel w
    , "speaker"    .= wiSpeaker w
    ]

-- | A sentence or paragraph with timing and word-level detail
data TimestampedText = TimestampedText
  { ttText       :: Text
  , ttStart      :: Int
  , ttEnd        :: Int
  , ttConfidence :: Double
  , ttWords      :: [WordInfo]
  , ttChannel    :: Maybe Text
  , ttSpeaker    :: Maybe Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON TimestampedText where
  parseJSON = withObject "TimestampedText" $ \o -> TimestampedText
    <$> o .:  "text"
    <*> o .:  "start"
    <*> o .:  "end"
    <*> o .:  "confidence"
    <*> o .:  "words"
    <*> o .:? "channel"
    <*> o .:? "speaker"

instance ToJSON TimestampedText where
  toJSON t = object
    [ "text"       .= ttText t
    , "start"      .= ttStart t
    , "end"        .= ttEnd t
    , "confidence" .= ttConfidence t
    , "words"      .= ttWords t
    , "channel"    .= ttChannel t
    , "speaker"    .= ttSpeaker t
    ]

-- | Response for the sentences endpoint
data SentencesResponse = SentencesResponse
  { srId            :: TranscriptId
  , srConfidence    :: Double
  , srAudioDuration :: Double
  , srSentences     :: [TimestampedText]
  } deriving stock (Show, Eq, Generic)

instance FromJSON SentencesResponse where
  parseJSON = withObject "SentencesResponse" $ \o -> SentencesResponse
    <$> o .: "id"
    <*> o .: "confidence"
    <*> o .: "audio_duration"
    <*> o .: "sentences"

instance ToJSON SentencesResponse where
  toJSON s = object
    [ "id"             .= srId s
    , "confidence"     .= srConfidence s
    , "audio_duration" .= srAudioDuration s
    , "sentences"      .= srSentences s
    ]

-- | Response for the paragraphs endpoint
data ParagraphsResponse = ParagraphsResponse
  { prId            :: TranscriptId
  , prConfidence    :: Double
  , prAudioDuration :: Double
  , prParagraphs    :: [TimestampedText]
  } deriving stock (Show, Eq, Generic)

instance FromJSON ParagraphsResponse where
  parseJSON = withObject "ParagraphsResponse" $ \o -> ParagraphsResponse
    <$> o .: "id"
    <*> o .: "confidence"
    <*> o .: "audio_duration"
    <*> o .: "paragraphs"

instance ToJSON ParagraphsResponse where
  toJSON p = object
    [ "id"             .= prId p
    , "confidence"     .= prConfidence p
    , "audio_duration" .= prAudioDuration p
    , "paragraphs"     .= prParagraphs p
    ]

-- | Subtitle format: SRT or VTT
data SubtitleFormat = SRT | VTT
  deriving stock (Show, Eq, Generic)

instance ToHttpApiData SubtitleFormat where
  toUrlPiece SRT = "srt"
  toUrlPiece VTT = "vtt"

-- | A match result from word search
data WordSearchMatch = WordSearchMatch
  { wsmText       :: Text
  , wsmCount      :: Int
  , wsmTimestamps :: [[Int]]
  , wsmIndexes    :: [Int]
  } deriving stock (Show, Eq, Generic)

instance FromJSON WordSearchMatch where
  parseJSON = withObject "WordSearchMatch" $ \o -> WordSearchMatch
    <$> o .: "text"
    <*> o .: "count"
    <*> o .: "timestamps"
    <*> o .: "indexes"

instance ToJSON WordSearchMatch where
  toJSON m = object
    [ "text"       .= wsmText m
    , "count"      .= wsmCount m
    , "timestamps" .= wsmTimestamps m
    , "indexes"    .= wsmIndexes m
    ]

-- | Response for the word search endpoint
data WordSearchResponse = WordSearchResponse
  { wsrId         :: TranscriptId
  , wsrTotalCount :: Int
  , wsrMatches    :: [WordSearchMatch]
  } deriving stock (Show, Eq, Generic)

instance FromJSON WordSearchResponse where
  parseJSON = withObject "WordSearchResponse" $ \o -> WordSearchResponse
    <$> o .: "id"
    <*> o .: "total_count"
    <*> o .: "matches"

instance ToJSON WordSearchResponse where
  toJSON w = object
    [ "id"          .= wsrId w
    , "total_count" .= wsrTotalCount w
    , "matches"     .= wsrMatches w
    ]

-- | Response for the redacted audio endpoint
data RedactedAudioResponse = RedactedAudioResponse
  { rarStatus           :: Text
  , rarRedactedAudioUrl :: Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON RedactedAudioResponse where
  parseJSON = withObject "RedactedAudioResponse" $ \o -> RedactedAudioResponse
    <$> o .: "status"
    <*> o .: "redacted_audio_url"

instance ToJSON RedactedAudioResponse where
  toJSON r = object
    [ "status"             .= rarStatus r
    , "redacted_audio_url" .= rarRedactedAudioUrl r
    ]

-- | Response from uploading a media file
newtype UploadedFile = UploadedFile
  { ufUploadUrl :: Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON UploadedFile where
  parseJSON = withObject "UploadedFile" $ \o -> UploadedFile
    <$> o .: "upload_url"

instance ToJSON UploadedFile where
  toJSON u = object ["upload_url" .= ufUploadUrl u]
