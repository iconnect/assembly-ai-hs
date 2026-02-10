{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module AssemblyAI.Types
  ( -- * Transcript Types
    Transcript (..)
  , TranscriptRequest (..)
  , TranscriptStatus (..)
  , TranscriptId (..)
  , AudioUrl (..)
    -- * API Key
  , ApiKey (..)
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

-- | Request to create a new transcript
data TranscriptRequest = TranscriptRequest
  { trAudioUrl :: AudioUrl
  } deriving stock (Show, Eq, Generic)

instance ToJSON TranscriptRequest where
  toJSON (TranscriptRequest url) = object ["audio_url" .= url]

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
