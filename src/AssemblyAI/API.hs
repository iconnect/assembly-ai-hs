{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AssemblyAI.API
  ( AssemblyAIAPI
  , assemblyAIAPI
  ) where

import AssemblyAI.Types (Transcript, TranscriptId, TranscriptRequest)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API (Capture, Get, Header', JSON, Post, ReqBody, Required, Strict, type (:<|>), type (:>))

-- | The AssemblyAI API for transcripts
type AssemblyAIAPI =
       Header' '[Required, Strict] "authorization" Text
    :> "v2" :> "transcript" :> ReqBody '[JSON] TranscriptRequest :> Post '[JSON] Transcript
  :<|> Header' '[Required, Strict] "authorization" Text
    :> "v2" :> "transcript" :> Capture "id" TranscriptId :> Get '[JSON] Transcript

-- | Proxy for the AssemblyAI API
assemblyAIAPI :: Proxy AssemblyAIAPI
assemblyAIAPI = Proxy
