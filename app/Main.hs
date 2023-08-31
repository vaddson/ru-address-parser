module Main where

import Control.Monad (forM_, unless)
import Control.Monad.Loops (whileM_)
import Data.Char (toLower)
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import qualified AddressParser as AP
import qualified IE
import qualified Parser as P

data Options = Options { optPretty :: Bool
                       , optJSON   :: Bool
                       , optHelp   :: Bool
                       }

defOptions :: Options
defOptions = Options { optPretty = False
                     , optJSON   = False
                     , optHelp   = False
                     }

options :: [OptDescr (Options -> IO Options)]
options = [ Option "p" ["pretty"]
              (NoArg (\opt -> return opt { optPretty = True }))
              "Pretty output."

          , Option "j" ["json"]
              (NoArg (\opt -> return opt { optJSON = True }))
              "Valuable JSON."

          , Option "h" ["help"]
              (NoArg (\_ -> do pname <- getProgName
                               hPutStrLn stderr (usageInfo pname options)
                               exitSuccess))
              "Print help."
          ]

getOptions :: IO Options
getOptions = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    foldl (>>=) (return defOptions) actions

main :: IO ()
main = withArgs
-- main = parseStdin
-- main = parseStdinPretty
-- main = jsonResult
-- main = jsonFullResult
-- main = jsonDebugResult
-- main = debug

withArgs :: IO ()
withArgs = do
    opts <- getOptions
    case opts of
        Options True  False _ -> parseStdin     IE.showOneAddressJsonPretty
        Options False True  _ -> parseStdinJson IE.showOneAddressJson
        Options True  True  _ -> parseStdinJson IE.showOneAddressJsonPretty
        _                     -> parseStdin     IE.showOneAddressJson

parseStdin :: ([AP.AddressData] -> String) -> IO ()
parseStdin printer = whileM_ (not <$> isEOF) $
    getLine >>= (putStrLn . printer . AP.parseAddress)

parseStdinJson :: ([AP.AddressData] -> String) -> IO ()
parseStdinJson printer = putStr "[" >> parsing seps >> putStrLn "]"
    where seps = "" : repeat ",\n"
          parsing (x:xs) = isEOF >>= flip unless
                           (putStr x >> process >> hFlush stdout >> parsing xs)
          process = getLine
                    >>= (putStr . printer . AP.parseAddress)

jsonResult :: IO ()
jsonResult = do
    input <- getInputString
    let res = AP.parseAddress input
    putStrLn $ IE.showOneAddressJson res

jsonFullResult :: IO ()
jsonFullResult = do
    input <- getInputString
    let res = AP.parseAddress input
    putStrLn $ IE.jsonDumpsFullPretty input res

jsonDebugResult :: IO ()
jsonDebugResult = do
    input <- getInputString
    putStrLn ""
    putStrLn $ "РАЗБИРАЕМ АДРЕС: \"" ++ input ++ "\""
    putStrLn ""
    let res = AP.parseAddress input
    putStrLn "ВАРИАНТЫ:"
    putStrLn $ IE.showAddressesJson res
    putStrLn ""
    putStrLn "ВЫБРАННЫЙ ВАРИАНТ:"
    putStrLn $ IE.showOneAddressJsonPretty res

debug :: IO ()
debug = do
    input <- getInputString
    putStrLn ""
    putStrLn $ "РАЗБИРАЕМ АДРЕС: \"" ++ input ++ "\""
    putStrLn ""
    let res = AP.parseAddress input
    putStrLn "ВАРИАНТЫ:"
    putStrLn $ AP.showAddresses res
    putStrLn ""
    putStrLn "ВЫБРАННЫЙ ВАРИАНТ:"
    putStrLn $ AP.showOneAddress res

getInputString :: IO String
getInputString = do
    input <- unwords <$> getArgs
    if null input then do
        let input = "вторая ул собачьей конуры, д1 вл 16б и что-то там еще"
        putStrLn ""
        putStrLn "ПОМОЩЬ. Запускаем так:"
        putStrLn "    address-parser <адрес дома>"
        putStrLn ""
        putStrLn $ "Ниже использован адрес для примера: '" ++ input ++ "'"
        putStrLn ""
        return input
    else
        return input
