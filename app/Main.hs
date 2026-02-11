{-# LANGUAGE OverloadedStrings #-}

module Main where

import AssemblyAI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Servant.Client (parseBaseUrl)
import System.Environment (getEnv)

main :: IO ()
main = do
  -- Get API key and audio path from environment variables
  apiKeyStr <- getEnv "ASSEMBLYAI_API_KEY"
  audioPath <- getEnv "TEST_AUDIO_PATH"
  let apiKey = ApiKey (T.pack apiKeyStr)

  -- Create client
  url <- parseBaseUrl "https://api.assemblyai.com"
  client <- mkAssemblyAIClient url apiKey

  -- Upload the local audio file
  fileBytes <- LBS.readFile audioPath
  uploadRes <- runAssemblyAI client (uploadFile client fileBytes)
  uploaded <- case uploadRes of
    Left err -> error $ "Upload failed: " ++ T.unpack err
    Right u  -> do
      putStrLn $ "Uploaded: " ++ T.unpack (ufUploadUrl u)
      pure u

  -- Submit for transcription using the uploaded URL
  let req = newTranscriptRequest (AudioUrl (ufUploadUrl uploaded)) [Universal3Pro, Universal2]
  res <- runAssemblyAI client (createTranscript client req)
  case res of
    Left err -> error $ "Transcription failed: " ++ T.unpack err
    Right t  -> print t
