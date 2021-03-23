{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Lazy as LBS
import qualified Control.Lens as Lens
import Control.Lens ((^.))
import qualified System.Console.GetOpt as GetOpt
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson.Pointer as JP
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.List as List
import qualified Control.Monad as Monad
import qualified Data.Yaml.Pretty as Yaml
import System.IO (hPrint, hPutStrLn, stderr, stdin, hGetContents, hClose)
import qualified System.Directory as Directory
import qualified System.Process.Typed as Process
import qualified Data.String as String

data ApiConfig = ApiConfig {
apiConfigFile :: String,
apiConfigDefaults :: InsOrd.InsOrdHashMap T.Text ConfigValue
}

instance Aeson.FromJSON ApiConfig where
  parseJSON = Aeson.withObject "ApiConfig" $ \v -> 
    ApiConfig <$> v Aeson..: "file"
              <*> v Aeson..: "defaults"

data ConfigValue = ConfigValue {
  configValueCommand :: T.Text
}

evaluateConfigValue :: ConfigValue -> IO T.Text
evaluateConfigValue (ConfigValue command) = do
  T.decodeUtf8 . LBS.toStrict <$> Process.readProcessStdout_ (Process.setStdin Process.inherit $ Process.setStderr Process.inherit $ Process.shell (T.unpack command))
    
  



instance Aeson.FromJSON ConfigValue where
  parseJSON = Aeson.withObject "ConfigValue" $ \v -> 
    ConfigValue <$> v Aeson..: "command"

readConfig :: IO (Maybe (InsOrd.InsOrdHashMap String ApiConfig))
readConfig = do
  configDirectory <- Directory.getXdgDirectory Directory.XdgConfig "api"
  let file = concat [configDirectory, "/config.yaml"]
  exists <- Directory.doesFileExist file
  if not exists then do
    putStrLn "Could not find config file"
    putStrLn file
    return Nothing
  else do
    contents <- BS.readFile file
    case Yaml.decodeEither' contents of
      Right config -> return (Just config)
      Left err -> return Nothing


someFunc :: IO ()
someFunc = do
  args <- getArgs
  allConf <- readConfig
  case args of
    file : command : args ->
      case allConf >>= InsOrd.lookup file of
        Just conf -> do
          configDirectory <- Directory.getXdgDirectory Directory.XdgConfig "api"
          let file = concat [configDirectory, "/", apiConfigFile conf]
          exists <- Directory.doesFileExist file
          if not exists then do
            putStrLn "Could not find file"
            putStrLn file
          else do
            contents <- BS.readFile file
            case Yaml.decodeEither' contents :: (Either Yaml.ParseException Aeson.Value) of
              Right document -> do
                case Aeson.fromJSON document of
                  Aeson.Success spec -> 
                    case spec ^. OpenApi.servers of
                      server : _ -> 
                          case findOperation spec (T.pack command) of
                            Just (path, _, operation) -> do
                              let parameters = Lens.view OpenApi.parameters operation
                                  dereferencedParameters = (map (dereferencePointer document) parameters) 
                              case Either.lefts dereferencedParameters of
                                [] -> do
                                  let parameters = Either.rights dereferencedParameters
                                      options = map paramaterToArgument parameters
                                      (arguments, non_options, errors) = GetOpt.getOpt GetOpt.Permute options args 
                                  defaults <- Monad.forM (InsOrd.toList (apiConfigDefaults conf)) $ \(key, value) -> do
                                    evaluateConfigValue value >>= \result -> return (key, result)

                                  let allArgs = defaults ++ arguments
                                    
                                  case (errors, (filterArgs parameters allArgs)) of
                                    ([], Right args) -> 
                                      runEndpoint server path operation parameters args
                                    ([], Left err) ->  do
                                      hPutStrLn stderr (T.unpack err)
                                      hPutStrLn stderr (GetOpt.usageInfo command options)
                                    (failures, Right _) -> do
                                      hPutStrLn stderr "failed to parse arguments"
                                      hPrint stderr failures
                                      hPutStrLn stderr (GetOpt.usageInfo command options)
                                    (failures, Left err) -> do
                                      hPutStrLn stderr "failed to parse arguments"
                                      hPrint stderr failures
                                      hPutStrLn stderr (T.unpack err)
                                      hPutStrLn stderr (GetOpt.usageInfo command options)

                                  
                                failures -> do
                                  hPutStrLn stderr "Failed to dereference pointers"
                                  hPrint stderr failures
                            _ -> do
                              hPutStrLn stderr "No such operation operations are:"
                              hPrint stderr (Lens.toListOf (OpenApi.allOperations . OpenApi.operationId) spec)

                      _ -> hPutStrLn stderr "Needs at least one server"
                  Aeson.Error err -> do
                    hPutStrLn stderr "failed to parse openApi"
                    print err
              Left err -> do
                hPutStrLn stderr "invalid YAML"
                hPrint stderr err
        Nothing -> do
          hPutStrLn stderr "No Such API, please add to config.yaml. Current apis are:"
          hPutStrLn stderr (Maybe.fromMaybe "Could not decode" (show . InsOrd.keys <$> allConf))
    _ -> 
      hPutStrLn stderr "No such command"

filterArgs :: [OpenApi.Param] -> [(T.Text, T.Text)] -> Either T.Text (InsOrd.InsOrdHashMap T.Text T.Text)
filterArgs params arguments = 
  let argumentDict = InsOrd.fromList arguments
      requiredParams = filter isRequired params
      requiredNames = map (^.OpenApi.name) requiredParams
      missingRequired = filter (not . flip InsOrd.member argumentDict) requiredNames
  in
  case missingRequired of
    [] -> 
      return argumentDict
    xs ->
      Left $ T.append "missing arguments: " (T.intercalate ", " xs)
 where
  isRequired :: OpenApi.Param -> Bool
  isRequired param = param ^.OpenApi.required == Just True || param ^. OpenApi.in_ == OpenApi.ParamPath
     
createRequest :: OpenApi.Server -> T.Text -> [OpenApi.Param] -> InsOrd.InsOrdHashMap T.Text T.Text -> IO Http.Request
createRequest server path params arguments = do
  let pathParams = filter ((==OpenApi.ParamPath) . (^. OpenApi.in_)) params 
      headerParams = filter ((==OpenApi.ParamHeader) . (^. OpenApi.in_)) params
      queryParams = filter ((==OpenApi.ParamQuery) . (^. OpenApi.in_)) params
      pathParamNames = map (^.OpenApi.name) pathParams
      pathSubstitutions = map (\name -> (name, Maybe.fromMaybe "" $ InsOrd.lookup name arguments)) pathParamNames
      headers = Maybe.catMaybes $ map (\name -> InsOrd.lookup name arguments >>= \value -> return (String.fromString $ T.unpack name, String.fromString $ T.unpack value)) (map (^.OpenApi.name) headerParams)
      queryString = T.intercalate "&"  . Maybe.catMaybes $ map (\name -> InsOrd.lookup name arguments >>= \value -> return $ T.concat [name, "=", value]) (map (^.OpenApi.name) queryParams)
  initialRequest <- Http.parseRequest (T.unpack $ T.concat [server ^. OpenApi.url, substitutePath path pathSubstitutions])
  return $ initialRequest { Http.requestHeaders = headers 
                          , Http.queryString = T.encodeUtf8 queryString }
  
  

dereferencePointer :: Aeson.FromJSON a => Aeson.Value -> OpenApi.Referenced a -> Either String a
dereferencePointer _ (OpenApi.Inline a) = Right a
dereferencePointer value (OpenApi.Ref (OpenApi.Reference text)) = 
 case JP.parsePointer text of
   Right pointer -> 
     case JP.valueAt pointer value of
       Just value -> 
         case Aeson.fromJSON value of
           Aeson.Success x -> Right x
           Aeson.Error err -> Left "Wrong type"
       Nothing -> Left "Could not find reference"
   Left err ->
     Left (show err)

paramaterToArgument :: OpenApi.Param -> GetOpt.OptDescr (T.Text, T.Text)
paramaterToArgument parameter =
  let required = parameter ^. OpenApi.required == Just True
      name = parameter ^.OpenApi.name
      description = Maybe.fromMaybe "" $ parameter ^. OpenApi.description :: T.Text
  in
    GetOpt.Option "" [T.unpack name] (GetOpt.ReqArg (\x -> (name, T.pack x)) "VALUE") (T.unpack (T.concat [description, if required then " (required)" else "" ]))


runEndpoint :: OpenApi.Server -> T.Text -> OpenApi.Operation -> [OpenApi.Param] -> InsOrd.InsOrdHashMap T.Text T.Text -> IO ()
runEndpoint server path operation parameters arguments = do
  hPutStrLn stderr $ "Calling operation: " ++ T.unpack (Maybe.fromMaybe "" (operation ^. OpenApi.operationId))
  hPutStrLn stderr $ "Calling path: " ++ T.unpack path
  man <- Http.newManager Https.tlsManagerSettings
  req <- createRequest server path parameters arguments
  hPrint stderr req
  response <- Http.httpLbs req man 
  case Aeson.decode $  Http.responseBody response :: Maybe Aeson.Value of
    Just body ->
      T.putStrLn $  T.decodeUtf8 $ Yaml.encodePretty Yaml.defConfig body
    Nothing -> 
      print (Http.responseBody response)




-- | Finds an operation with a given operationId. Then returns a triple
--   containing it's path, it's path item and the operation
findOperation :: OpenApi.OpenApi -> T.Text -> Maybe (T.Text, OpenApi.PathItem, OpenApi.Operation)
findOperation spec operationId =
  let keys = InsOrd.toList (spec ^. OpenApi.paths)
  in
    foldl (\acc (key, pathItem) -> 
      case acc of
        Just value -> Just value
        Nothing -> 
          Monad.join $ List.find Maybe.isJust $ List.map (\method -> do
                operation <- (pathItem ^. method)
                if (operation ^. OpenApi.operationId) == Just operationId then
                  Just (T.pack key, pathItem, operation)
                else
                  Nothing
              ) [OpenApi.get, OpenApi.post, OpenApi.patch, OpenApi.delete, OpenApi.options, OpenApi.head_, OpenApi.trace] :: Maybe (T.Text, OpenApi.PathItem, OpenApi.Operation)
            ) Nothing keys 

substitutePath :: T.Text -> [(T.Text, T.Text)] -> T.Text
substitutePath original [] = original
substitutePath original ((from, with): rest) = 
    substitutePath result rest
  where
     result :: T.Text
     result = T.replace (T.concat ["{", from, "}"]) with original
