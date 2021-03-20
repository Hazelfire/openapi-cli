module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Https
import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as BS
import qualified Control.Lens as Lens
import Control.Lens ((^.))
import qualified System.Console.GetOpt as GetOpt


someFunc :: IO ()
someFunc = do
  args <- getArgs
  case args of
    file : "call" : _ -> do
      contents <- BS.readFile file
      case Yaml.decodeEither' contents of
        Right spec -> do
          let operations = spec ^. OpenApi.allOperations
              parameters = operations ^. OpenApi.parameters
          print operations
        Left err -> do
          putStrLn "failed to parse openApi"
          print err
    _ -> 
      putStrLn "No such command"
