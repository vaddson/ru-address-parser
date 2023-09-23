module AddressParser ( AddressParser(..)
                     , AddressData(..)
                     , adderessDataRealTown
                     , addressDataHouseInt
                     , estimate
                     , selectAddress
                     , selectFirstAddress
                     , selectBestAddress
                     , address
                     , rest
                     , restSepBy
                     , rests
                     , parseAddress
                     , showAddresses
                     , showOneAddress
                     ) where

import AddressParser.Internal
import Control.Applicative hiding (many)
import Data.Char (isDigit, isSpace)
import Data.Maybe
import Parser
import Data.List (intercalate, sort, maximumBy)
import Control.Monad (guard)
import Data.Either (isRight, fromRight)

type AddressParser = Parser

data AddressData = AddressData { addressDataCountry    :: Maybe String
                               , addressDataSubcountry :: Maybe [Subcountry]
                               , addressDataTown       :: Maybe String
                               , addressDataSubtown    :: Maybe [Subtown]
                               , addressDataStreet     :: Maybe String
                               , addressDataHouse      :: Maybe [String]
                               } deriving Eq

adderessDataRealTown :: AddressData -> Maybe String
adderessDataRealTown ad = select ((lastTown . filterTowns . addressDataSubtown) ad)
                                 (addressDataTown ad)
    where select (Just subtown) _    = Just subtown
          select Nothing (Just town) = Just town
          select Nothing Nothing     = Nothing
          lastTown [] = Nothing
          lastTown xs = Just $ last xs
          filterTowns (Just xs) = map name $ filter ((/="") . name) xs
          filterTowns Nothing   = []
          name (RealTown   text) = text
          name (Colony     text) = text
          name (Settlement text) = text
          name (Selo       text) = text
          name (Village    text) = text
          name _                 = ""

addressDataHouseInt :: AddressData -> Maybe Int
addressDataHouseInt ad = maybeInt =<< addressDataHouse ad
    where maybeInt parts  = case candidates parts of
                                []     -> Nothing
                                (x:xs) -> Just $ read $ head x
          candidates = filter (/=[]) . map ints
          ints       = filter (/="") . words . replace
          replace    = map (\c -> if isDigit c then c else ' ')

quoteString :: String -> String
quoteString x = "'" ++ x ++ "'"

instance Show AddressData where
    show ad = "country='"        ++ fromMaybe "" (addressDataCountry ad)
              ++ "', subcountry=[" ++ listToStr (fromMaybe []
                                                           (addressDataSubcountry ad))
              ++ "], town='"     ++ fromMaybe "" (addressDataTown ad)
              ++ "', subtown=["  ++ listToStr (fromMaybe [] (addressDataSubtown ad))
              ++ "], realTown='" ++ fromMaybe "" (adderessDataRealTown ad)
              ++ "', street='"   ++ fromMaybe "" (addressDataStreet ad)
              ++ "', house=["
              ++ intercalate ", " (maybe [] (map quoteString) (addressDataHouse ad))
              ++ "], houseInt="  ++ show (fromMaybe 0 (addressDataHouseInt ad))

showAddressData :: AddressData -> String
showAddressData ad = "country    = '" ++ fromMaybe "" (addressDataCountry ad)
    ++ "'\nsubcountry = [" ++ listToStr (fromMaybe [] (addressDataSubcountry ad))
    ++ "]\ntown       = '" ++ fromMaybe "" (addressDataTown ad)
    ++ "'\nsubtown    = [" ++ listToStr (fromMaybe [] (addressDataSubtown ad))
    ++ "]\nrealTown   = '" ++ fromMaybe "" (adderessDataRealTown ad)
    ++ "'\nstreet     = '" ++ fromMaybe "" (addressDataStreet ad)
    ++ "'\nhouse      = ["
    ++ intercalate ", " (maybe [] (map quoteString) (addressDataHouse ad))
    ++ "]\nhouseInt   = " ++ show (fromMaybe 0 (addressDataHouseInt ad))

def :: AddressData
def = AddressData { addressDataCountry    = Nothing
                  , addressDataSubcountry = Nothing
                  , addressDataTown       = Nothing
                  , addressDataSubtown    = Nothing
                  , addressDataStreet     = Nothing
                  , addressDataHouse      = Nothing
                  }

estimate :: AddressData -> Int
estimate ad = len   (addressDataSubcountry ad)
              + len (addressDataSubtown    ad)
              + len (addressDataHouse      ad)
              + count [ addressDataCountry ad
                      , addressDataTown    ad
                      , addressDataStreet  ad ]
    where count = length . filter isJust
          len (Just xs) = length xs
          len Nothing   = 0

selectBestAddress :: [AddressData] -> AddressData
selectBestAddress = maximumBy cmp
    where cmp a b = if estimate a >= estimate b then GT else LT

selectFirstAddress :: [AddressData] -> AddressData
selectFirstAddress = head

selectAddress :: [AddressData] -> AddressData
selectAddress = selectFirstAddress

address :: Parser AddressData
address =
          -- С номером дома:
          AddressData     <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> (Just <$> (sep *> townName))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> (Just <$> countryName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> townName))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> (Just <$> countryName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> (Just <$> countryName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> (Just <$> (sep *> townName))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> townName)
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> townName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> townName)
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> subtownsNamesTypes)
                          <*> (Just <$> (sep *> streetName))
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> subtownsNamesTypes)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> houseNumberParts))
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> streetName)
                          <*> (Just <$> (sep *> houseNumberParts))
          -- Без номера дома:
          <|> AddressData <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> (Just <$> (sep *> townName))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes <* end))
                          <*> insertion Nothing
                          <*> insertion Nothing
          <|> AddressData <$> (Just <$> countryName)
                          <*> (Just <$> (sep *> subcountriesNamesTypes))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> subtownsNamesTypes <* end))
                          <*> insertion Nothing
                          <*> insertion Nothing
          <|> AddressData <$> (Just <$> countryName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> (Just <$> countryName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> townName))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> (Just <$> countryName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> (Just <$> (sep *> townName))
                          <*> (Just <$> (sep *> subtownsNamesTypes <* end))
                          <*> insertion Nothing
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> (Just <$> (sep *> townName))
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> (Just <$> subcountriesNamesTypes)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> subtownsNamesTypes <* end))
                          <*> insertion Nothing
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> townName)
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> townName)
                          <*> insertion Nothing
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> townName)
                          <*> (Just <$> (sep *> subtownsNamesTypes))
                          <*> insertion Nothing
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> subtownsNamesTypes)
                          <*> (Just <$> (sep *> streetName <* end))
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> (subtownsNamesTypes <* end))
                          <*> insertion Nothing
                          <*> insertion Nothing
          <|> AddressData <$> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> insertion Nothing
                          <*> (Just <$> (streetName <* end))
                          <*> insertion Nothing
    where sep = comma <|> whitespace1
          end = comma <|> endOfString

rest :: Parser a -> Parser String
rest (Parser p) = Parser p2
    where p2 s = convert (p s)
          convert (Right (s2, val)) = Right (s2, s2)
          convert (Left s3) = Left s3

restSepBy :: Parser a -> Parser b -> Parser [String]
restSepBy p sep = ((same <* p) <:> terms)
                  <|> terms
    where same = Parser (\s -> Right (s, s))
          terms = many (rest sep <* p)

rests :: String -> [String]
rests s = convert $ parse (part `restSepBy` separator) s
    where convert (Right (_, parts)) = parts
          convert _ = []
          part = rest $ many1 $ satisfy $ not . sepChar
          separator = comma <|> whitespace1
          sepChar c = c == ',' || isSpace c

parseAddress :: String -> [AddressData]
parseAddress s = [ snd (fromRight ("", def) addr)
                 | cand <- rests s
                 , let addr = parse address cand
                 , isRight addr]

showAddresses :: Show a => [a] -> String
showAddresses addresses = intercalate "\n"
                          $ zipWith (curry showPair) [1..]
                          $ map show addresses
    where showPair p = show (fst p) ++ ". " ++ snd p

showOneAddress :: [AddressData] -> String
showOneAddress [] = "(no address)"
showOneAddress xs = showAddressData $ selectAddress xs
