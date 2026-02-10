# assembly-ai-hs

A Haskell client library for the [AssemblyAI](https://www.assemblyai.com) API, built with Servant.

## Features

- Type-safe API client using Servant
- Support for transcript creation and retrieval
- Built for GHC 9.6.7
- Clean, idiomatic Haskell API

## Installation

Add `assembly-ai` to your project's dependencies in your `.cabal` file:

```cabal
build-depends: assembly-ai
```

## Usage

```haskell
import AssemblyAI
import AssemblyAI.Client
import AssemblyAI.Types
import qualified Data.Text as T

main :: IO ()
main = do
  -- Initialize client with your API key
  let apiKey = ApiKey "your-api-key-here"
  client <- mkAssemblyAIClient apiKey
  
  -- Create a transcript
  let request = TranscriptRequest (AudioUrl "https://example.com/audio.mp3")
  result <- runAssemblyAI client (createTranscript apiKey request)
  
  case result of
    Left err -> putStrLn $ "Error: " ++ T.unpack err
    Right transcript -> do
      putStrLn $ "Transcript created with ID: " ++ show (tId transcript)
      putStrLn $ "Status: " ++ show (tStatus transcript)
```

## Building

This project requires GHC 9.6.7. Build with Cabal:

```bash
cabal build
```

## Running the Example

```bash
export ASSEMBLYAI_API_KEY="your-api-key"
cabal run assembly-ai-example
```

## API Reference

See the [AssemblyAI API documentation](https://www.assemblyai.com/docs/api-reference/transcripts) for more information about the available endpoints and data types.

## License

BSD-3-Clause
