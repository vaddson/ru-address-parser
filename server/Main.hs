{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Network.Wai ( Application
                   , Response
                   , Middleware
                   , responseLBS
                   , requestMethod
                   , rawPathInfo
                   , rawQueryString
                   , responseStatus, Request (queryString))
import Network.HTTP.Types ( Status
                          , status200
                          , hContentType
                          , badRequest400
                          , methodGet
                          , statusCode)
import Network.HTTP.Types.URI
import Network.Wai.Handler.Warp (run)
import System.Console.GetOpt
import System.Environment

import qualified AddressParser as AP
import qualified IE

newtype AppOptions = AppOptions { appPort :: Int }

defAppOptions :: AppOptions
defAppOptions = AppOptions { appPort = 8000 }

appOptions :: [OptDescr (AppOptions -> IO AppOptions)]
appOptions = [ Option "p" ["port"]
                 (ReqArg (\arg opt -> return opt { appPort = read arg })
                         "PORT")
                 "Listener port." ]

getOptions :: IO AppOptions
getOptions = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder appOptions args
    foldl (>>=) (return defAppOptions) actions

main :: IO ()
main = do
    opts <- getOptions
    putStrLn "Serving..."
    run (appPort opts) $ withLogging application

data ReqOptions = ReqOptions { optQuery      :: String
                             , optPretty     :: Bool
                             , optMeditative :: Bool }

defReqOptions = ReqOptions { optQuery      = ""
                           , optPretty     = False
                           , optMeditative = False }

reqOptions :: Request -> ReqOptions
reqOptions req = ( extractQuery
                 . extractPretty
                 . extractMeditative ) defReqOptions
    where optMap = M.fromList $ queryString req
          extractQuery      opts =
              case M.lookup "q" optMap of
                  Just (Just text) -> opts { optQuery  = convert text }
                  _                -> opts
          extractPretty     opts
              | M.member "pretty"     optMap = opts { optPretty     = True }
              | otherwise                    = opts
          extractMeditative opts
              | M.member "meditative" optMap = opts { optMeditative = True }
              | otherwise                    = opts
          convert = T.unpack . TE.decodeUtf8 . urlDecode False

withLogging :: Middleware
withLogging app req respond =
    app req $ \response -> do
        putStrLn $ method ++ " " ++ statusOf response ++ " " ++ query
        respond response
    where method   = (BS.unpack . requestMethod) req
          query    = T.unpack $ TE.decodeUtf8 $ urlDecode False
                   $ BS.concat [rawPathInfo req, rawQueryString req]
          statusOf = show . statusCode . responseStatus

application :: Application
application req respond | isRoute methodGet "" = respond
                          $ responseLBS status200
                                        [(hContentType, "application/json")]
                                        "{ \"RU address parser.\" }"
                        | isRoute methodGet "parse" = respond
                          $ responseLBS status200
                                        [(hContentType, "application/json")]
                                        (convert (parseRuAddress (reqOptions req)))
                        | otherwise = respond
                          $ responseLBS badRequest400
                                        [(hContentType, "application/json")]
                                        "{ \"ERROR\" }"
    where isRoute method routPath = requestMethod req == method
                                    && routPath == path
          query   = rawQueryString req
          path    = BS.tail $ rawPathInfo req
          convert = LTE.encodeUtf8 . LT.pack

parseRuAddress :: ReqOptions -> String
parseRuAddress opts | optPretty opts       && optMeditative opts
                      = IE.showBestAddressJsonPretty $ AP.parseAddress $ optQuery opts
                    | optPretty opts       && not (optMeditative opts)
                      = IE.showOneAddressJsonPretty  $ AP.parseAddress $ optQuery opts
                    | not (optPretty opts) && optMeditative opts
                      = IE.showBestAddressJson       $ AP.parseAddress $ optQuery opts
                    | otherwise
                      = IE.showOneAddressJson        $ AP.parseAddress $ optQuery opts
