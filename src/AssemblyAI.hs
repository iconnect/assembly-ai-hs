-- | AssemblyAI Haskell Client Library
--
-- This library provides a Servant-based client for the AssemblyAI API.
--
-- Example usage:
--
-- @
-- import AssemblyAI
-- import AssemblyAI.Types
-- import AssemblyAI.Client
--
-- main :: IO ()
-- main = do
--   let apiKey = ApiKey "your-api-key"
--   client <- mkAssemblyAIClient apiKey
--   let request = TranscriptRequest (AudioUrl "https://example.com/audio.mp3")
--   result <- runAssemblyAI client (createTranscript apiKey request)
--   case result of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right transcript -> print transcript
-- @
module AssemblyAI
  ( -- * Re-exports
    module AssemblyAI.Types
  , module AssemblyAI.API
  , module AssemblyAI.Client
  ) where

import AssemblyAI.API
import AssemblyAI.Client
import AssemblyAI.Types
