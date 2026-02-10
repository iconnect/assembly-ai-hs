{-# LANGUAGE OverloadedStrings #-}

module Main where

import AssemblyAI
import System.Environment (getEnv)
import qualified Data.Text as T

main :: IO ()
main = do
  -- Get API key from environment variable
  apiKeyStr <- getEnv "ASSEMBLYAI_API_KEY"
  let apiKey = ApiKey (T.pack apiKeyStr)
  
  -- Create client
  _ <- mkAssemblyAIClient apiKey
  
  putStrLn "AssemblyAI Haskell Client Example"
  putStrLn "=================================="
  putStrLn ""
  putStrLn "This example demonstrates how to use the AssemblyAI library."
  putStrLn ""
  putStrLn "To create a transcript:"
  putStrLn "  let request = TranscriptRequest (AudioUrl \"https://example.com/audio.mp3\")"
  putStrLn "  result <- runAssemblyAI client (createTranscript apiKey request)"
  putStrLn ""
  putStrLn "To get a transcript:"
  putStrLn "  result <- runAssemblyAI client (getTranscript apiKey (TranscriptId \"transcript-id\"))"
  putStrLn ""
  putStrLn "Client initialized successfully!"
