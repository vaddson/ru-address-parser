module AddressParser.Internal ( comma
                              , block
                              , blockNumber
                              , endOfBlockWord
                              , complexBlockNumber
                              , endOfHouseWord
                              , houseKey
                              , houseNumberPart
                              , houseNumberParts
                              , houseNumber
                              , streetKey
                              , ruWord
                              , ruWord1
                              , complexRuWord
                              , complexRuWord1
                              , numerical
                              , essenceNameByWords
                              , essenceNameByWordsComplex
                              , essenceNameWithKey
                              , essenceNameWithKeyLeft
                              , streetNameWithSeveralKeys
                              , streetNameWithKilometre
                              , streetNameWithKey
                              , streetName
                              , essenceSeparator
                              , town
                              , subtownKey
                              , anySubtownName
                              , settlementName
                              , subtownName
                              , realTownName
                              , subtownsNames
                              , Subtown(..)
                              , listToStr
                              , subtownsNamesTypes
                              , endOfTownNameWithoutKey
                              , townName
                              , regionName
                              , republicName
                              , Subcountry(..)
                              , subcountriesNamesTypes
                              , countryName
                              ) where

import Control.Applicative hiding (many)
import Data.Char (isDigit, isSpace, toLower)
import Parser
import Data.List (intercalate, sort)
import Control.Monad (guard)
import Data.Either (isRight, fromRight)

toList :: a -> [a]
toList x = [x]

comma :: Parser String
comma = whitespace *> char ',' <:> whitespace

ruLetter :: Parser Char
ruLetter = satisfy f
    where f c = 'а' <= lowc && lowc <= 'я' || lowc == 'ё'
              where lowc = toLower c

block :: Parser String
block = string     "б/н"  <* endOfBlockWord
        <|> string "блок" <* endOfBlockWord
        <|> string "зона" <* endOfBlockWord
        <|> string "аб"   <* endOfBlockWord
        <|> string "вк"   <* endOfBlockWord
        <|> (toList <$> ruLetter) <* endOfBlockWord

blockNumber :: Parser String
blockNumber = (number <++> block) <|> number <|> block

complexBlockNumber :: Parser String
complexBlockNumber = checkNot numerical *> (mconcat <$> numbers)
    where numbers = blockNumber <:> many oneMore
          oneMore = (char '/' <|> char '-') <:> blockNumber

lot :: Parser String
lot = "участок" <$ (string "участок" <|> string "уч")

landLot :: Parser String
landLot = string "земельный" <++> whitespace1 <++> lot

crossing :: Parser String
crossing = "пересечение" <$ (string "пересечение" <|> string "перес")

house :: Parser String
house = "дом" <$ (string "дом" <|> string "д")

estate :: Parser String
estate = "владение" <$ (string "владение" <|> string "влад" <|> string "вл")

houseEatate :: Parser String
houseEatate = "домовладение" <$ (string "домовладение" <|> string "домовл")

garage :: Parser String
garage = "гараж" <$ (string "гараж" <|> string "гар")

edifice :: Parser String
edifice = "здание" <$ (string "здание" <|> string "здан" <|> string "зд" <|> string "з")

corps :: Parser String
corps = "корпус" <$ (string "корпус" <|> string "корп" <|> string "кор" <|> string "к")

erection :: Parser String
erection = "сооружение" <$ (string "сооружение" <|> string "соор")

building :: Parser String
building = "строение" <$ (string "строение" <|> string "строен" <|> string "стр" <|> string "с")

litera :: Parser String
litera = "литера" <$ (string "литера" <|> string "лит")

endOfBlockWord :: Parser String
endOfBlockWord = Parser p
    where p "" = Right ("", "")
          p (x:xs) | isSpace x || x == '.' || x == ',' || x == '-' || x == '/' || isDigit x = Right (x:xs, [x])
                   | otherwise = Left "unexpected end of block word"

endOfHouseWord :: Parser String
endOfHouseWord = Parser p
    where p "" = Right ("", "")
          p (x:xs) | isSpace x || x == ',' || isDigit x = Right (x:xs, [x])
                   | x == '.' = Right (xs, [x])
                   | otherwise = Left "unexpected end of house word"

houseKey :: Parser String
houseKey = house           <* endOfHouseWord
            <|> estate      <* endOfHouseWord
            <|> houseEatate <* endOfHouseWord
            <|> corps       <* endOfHouseWord
            <|> building    <* endOfHouseWord
            <|> edifice     <* endOfHouseWord
            <|> erection    <* endOfHouseWord
            <|> landLot     <* endOfHouseWord
            <|> lot         <* endOfHouseWord
            <|> garage      <* endOfHouseWord
            <|> crossing    <* endOfHouseWord
            <|> litera      <* endOfHouseWord

separatorHouse :: Parser String
separatorHouse = whitespace *> (char ',' <:> whitespace) <|> whitespace

houseNumberPart :: Parser String
houseNumberPart = houseKey <++> whitespace <++> complexBlockNumber

houseNumberParts :: Parser [String]
houseNumberParts = checkNot (district <|> area <|> highway)
                   *> (varLikeYandex1 <|> varLikeYandex2 <|> var1 <|> var2)
                   <* endOfHouseWord
    where var1 = houseNumberPart <:> many oneMore
          var2 = (("дом " ++) <$> complexBlockNumber) <:> many oneMore
          varLikeYandex1 = (("дом " ++) <$> (fraction <|> number))
                           <:> ((houseKey <++> whitespace <++> complexBlockNumber)
                                <:> many oneMore)
          varLikeYandex2 = (houseKey <++> whitespace <++> (fraction <|> number))
                           <:> ((houseKey <++> ((" " ++) <$> complexBlockNumber))
                                <:> many oneMore)
          fraction = blockNumber <++> ((char '/' <|> char '-') <:> number)
          oneMore = separatorHouse *> houseNumberPart

houseNumber :: Parser String
houseNumber = intercalate ", " <$> houseNumberParts

endOfEssenceKey :: Parser String
endOfEssenceKey = Parser p
    where p "" = Right ("", "")
          p (x:xs) | isSpace x || x == ',' = Right (x:xs, [x])
                   | x == '.' = Right (xs, [x])
                   | otherwise = Left "unexpected end of street key"

ofName :: Parser String
ofName = ("имени" <$ (string "имени" <|> string "им")) <* endOfEssenceKey

area :: Parser String
area = ("квартал" <$ (string "квартал" <|> string "кварт" <|> string "кв-л")) <* endOfEssenceKey

street :: Parser String
street = ("улица" <$ (string "улица" <|> string "ул")) <* endOfEssenceKey

passage :: Parser String
passage = ("проезд" <$ (string "проезд" <|> string "пр")) <* endOfEssenceKey

projectedPassage :: Parser String
projectedPassage = string "проектируемый" <++> whitespace1 <++> passage

alleyway :: Parser String
alleyway = ("переулок" <$ (string "переулок" <|> string "пер")) <* endOfEssenceKey

deadEnd :: Parser String
deadEnd = ("тупик" <$ (string "тупик" <|> string "туп")) <* endOfEssenceKey

highway :: Parser String
highway = ("шоссе" <$ (string "шоссе" <|> string "шос" <|> string "ш")) <* endOfEssenceKey

avenue :: Parser String
avenue = ("проспект" <$ (string "проспект" <|> string "просп" <|> string "пр-т")) <* endOfEssenceKey

space :: Parser String
space = ("площадь" <$ (string "площадь" <|> string "пл")) <* endOfEssenceKey

quay :: Parser String
quay = ("набережная" <$ (string "набережная" <|> string "наб")) <* endOfEssenceKey

boulevar :: Parser String
boulevar = ("бульвар" <$ (string "бульвар" <|> string "бульв" <|> string "бул" <|> string "б-р")) <* endOfEssenceKey

big :: Parser String
big = (string "большой" <|> string "большая" <|> "бол." <$ (string "бол" <|> string "б")) <* endOfEssenceKey

small :: Parser String
small = (string "малый" <|> string "малая" <|> "мал." <$ (string "мал" <|> string "м")) <* endOfEssenceKey

kilometre :: Parser String
kilometre = ("километр" <$ (string "километр" <|> string "км")) <* endOfEssenceKey

alley :: Parser String
alley = ("аллея" <$ (string "аллея" <|> string "ал")) <* endOfEssenceKey

overpass :: Parser String
overpass = ("эстакада" <$ (string "эстакада" <|> string "эст")) <* endOfEssenceKey

right :: Parser String
right = (string "правый" <|> string "правая" <|> "прав." <$ string "прав") <* endOfEssenceKey

left :: Parser String
left = (string "левый" <|> string "левая" <|> "лев." <$ string "лев") <* endOfEssenceKey

side :: Parser String
side = (string "сторона" <|> string "стор" <|> string "ст") <* endOfEssenceKey

line :: Parser String
line = string "линия" <* endOfEssenceKey

khutor :: Parser String
khutor = string "хутор" <* endOfEssenceKey

bridge :: Parser String
bridge = string "мост" <* endOfEssenceKey

platform :: Parser String
platform = string "платформа" <* endOfEssenceKey

station :: Parser String
station = string "станция" <* endOfEssenceKey

microDistrict :: Parser String
microDistrict = ("микрорайон" <$ (string "микрорайон" <|> string "микрор-н" <|> string "мр-н")) <* endOfEssenceKey

metroTown :: Parser String
metroTown = string "Метрогородок" <* endOfEssenceKey

kremlin :: Parser String
kremlin = string "кремль" <* endOfEssenceKey

mkad :: Parser String
mkad = string "мкад" <* endOfEssenceKey

numberSymbol :: Parser String
numberSymbol = "№" <$ (string "n" <|> string "№" <|> string "#" <|> string "номер")

streetKey :: Parser String
streetKey = ofName
            <|> street
            <|> area
            <|> projectedPassage
            <|> passage
            <|> alleyway
            <|> deadEnd
            <|> highway
            <|> avenue
            <|> space
            <|> quay
            <|> boulevar
            <|> alley
            <|> overpass
            <|> side
            <|> line
            <|> khutor
            <|> bridge
            <|> platform
            <|> station
            <|> microDistrict
            <|> kremlin
            <|> mkad
            <|> railway
            <|> autoRoad

endOfRuWord :: Parser String
endOfRuWord = Parser p
    where p "" = Right ("", "")
          p (x:xs) | isSpace x || x == ',' || x == '-' = Right (x:xs, [x])
                   | otherwise = Left "unexpected end of street key"

ruWord :: Parser String
ruWord = many (checkNot numerical *> ruLetter) <++> option (string ".") <* endOfRuWord

ruWord1 :: Parser String
ruWord1 = many1 (checkNot numerical *> ruLetter) <++> option (string ".") <* endOfRuWord

quotedText :: Parser String
quotedText = quote '"' <|> quote '\''
    where quote ch = char ch <:> many (satisfy (/=ch)) <++> (toList <$> char ch)

complexRuWord1 :: Parser String
complexRuWord1 = var1 <++> endByNumber
    where var1        = intercalate "-" <$> (anyWord `sepBy` char '-')
          anyWord     = ruWord1 <|> quotedText
          endByNumber = option (string "-" <++> number) <++> option (string "\"")

complexRuWord :: Parser String
complexRuWord = option complexRuWord1

numerical :: Parser String
numerical = "1-й" <$ (string "первый" <|> string "1-ый" <|> string "1-й")
            <|> "1-я" <$ (string "первая" <|> string "1-ая" <|> string "1-я")
            <|> "2-й" <$ (string "второй" <|> string "2-ой" <|> string "2-й")
            <|> "2-я" <$ (string "вторая" <|> string "2-ая" <|> string "2-я")
            <|> "3-й" <$ (string "третий" <|> string "3-ий" <|> string "3-й")
            <|>  "3-я"<$ (string "третья" <|> string "3-я")
            <|> "4-й" <$ (string "четвертый" <|> string "4-ый" <|> string "4-й")
            <|> "4-я" <$ (string "четвертая" <|> string "4-ая" <|> string "4-я")
            <|> "5-й" <$ (string "пятый" <|> string "5-ый" <|> string "5-й")
            <|> "5-я" <$ (string "пятая" <|> string "5-ая" <|> string "5-я")
            <|> "6-й" <$ (string "шестой" <|> string "6-ой" <|> string "6-й")
            <|> "6-я" <$ (string "шестая" <|> string "6-ая" <|> string "6-я")
            <|> "7-й" <$ (string "седьмой" <|> string "7-ой" <|> string "7-й")
            <|> "7-я" <$ (string "седьмая" <|> string "7-ая" <|> string "7-я")
            <|> "8-й" <$ (string "восьмой" <|> string "8-ой" <|> string "8-й")
            <|> "8-я" <$ (string "восьмая" <|> string "8-ая" <|> string "8-я")
            <|> "9-й" <$ (string "девятый" <|> string "9-ый" <|> string "9-й")
            <|> "9-я" <$ (string "девятая" <|> string "9-ая" <|> string "9-я")
            <|> "10-й" <$ (string "десятый" <|> string "10-ый" <|> string "10-й")
            <|> "10-я" <$ (string "десятая" <|> string "10-ая" <|> string "10-я")
            <|> "11-й" <$ (string "одиннадцатый" <|> string "11-ый" <|> string "11-й")
            <|> "11-я" <$ (string "одиннадцатая" <|> string "11-ая" <|> string "11-я")
            <|> "12-й" <$ (string "двенадцатый" <|> string "12-ый" <|> string "12-й")
            <|> "12-я" <$ (string "двенадцатая" <|> string "12-ая" <|> string "12-я")
            <|> "13-й" <$ (string "тринадцатый" <|> string "13-ый" <|> string "13-й")
            <|> "13-я" <$ (string "тринадцатая" <|> string "13-ая" <|> string "13-я")
            <|> "14-й" <$ (string "четырнадцатый" <|> string "14-ый" <|> string "14-й")
            <|> "14-я" <$ (string "четырнадцатая" <|> string "14-ая" <|> string "14-я")
            <|> "15-й" <$ (string "пятнадцатый" <|> string "15-ый" <|> string "15-й")
            <|> "15-я" <$ (string "пятнадцатая" <|> string "15-ая" <|> string "15-я")
            <|> "16-й" <$ (string "шестнадцатый" <|> string "16-ый" <|> string "16-й")
            <|> "16-я" <$ (string "шестнадцатая" <|> string "16-ая" <|> string "16-я")
            <|> "17-й" <$ (string "семнадцатый" <|> string "17-ый" <|> string "17-й")
            <|> "17-я" <$ (string "семнадцатая" <|> string "17-ая" <|> string "17-я")
            <|> "18-й" <$ (string "восемнадцатый" <|> string "18-ый" <|> string "18-й")
            <|> "18-я" <$ (string "восемнадцатая" <|> string "18-ая" <|> string "18-я")
            <|> "19-й" <$ (string "девятнадцатый" <|> string "19-ый" <|> string "19-й")
            <|> "19-я" <$ (string "девятнадцатая" <|> string "19-ая" <|> string "19-я")
            <|> "20-й" <$ (string "двадцатый" <|> string "20-ый" <|> string "20-й")
            <|> "20-я" <$ (string "двадцатая" <|> string "20-ая" <|> string "20-я")
            <|> number <++> (("-й" <$ (string "-ый" <|> string "-ой" <|> string "-й"))
                             <|> ("-я" <$ (string "-ая" <|> string "-я")))

numericSpecial :: Parser String
numericSpecial = number <++> option (string "-летия" <|> string "-ти" <|> string "-и" <|> string "-го")

nameWordOfEssence :: Parser String -> Parser String
nameWordOfEssence key = checkNot (key <|> commonKey <|> numberSymbol)
                        *> complexRuWord

nameWordOfEssence1 :: Parser String -> Parser String
nameWordOfEssence1 key = checkNot (key <|> commonKey <|> numberSymbol)
                         *> complexRuWord1

essenceNameByWords :: Parser String -> Parser a -> Parser String
essenceNameByWords key stop = unwords <$>
                              (checkNot stop *> nameWordOfEssence1 key) `sepBy` whitespace1

essenceNameByWordsComplexLeft :: Parser String -> Parser a -> Parser String
essenceNameByWordsComplexLeft key stop = var1 <|> var2 <|> var3
    where var1 = option (essenceNameByWords key stop <++> whitespace1)
                 <++> ofName <++> whitespace
                 <++> option (essenceNameByWords key stop <++> whitespace1)
                 <++> numericSpecial <++> whitespace1 <++> essenceNameByWords key stop
          var2 = option (essenceNameByWords key stop <++> whitespace1)
                 <++> ofName <++> whitespace <++> essenceNameByWords key stop
          var3 = option (essenceNameByWords key stop <++> whitespace1)
                 <++> numericSpecial <++> whitespace1 <++> essenceNameByWords key stop

essenceNameByWordsComplexRight :: Parser String -> Parser a -> Parser String
essenceNameByWordsComplexRight = essenceNameByWords

essenceNameByWordsComplex :: Parser String -> Parser a -> Parser String
essenceNameByWordsComplex key stop = essenceNameByWordsComplexLeft key stop
                                     <|> essenceNameByWordsComplexRight key stop

essenceNameWithKeyLeft :: Parser String -> Parser a -> Parser String
essenceNameWithKeyLeft key stop = var1 <|> var2
    where var1 = key <++> whitespace <++> essenceNameByWordsComplex key stop
                 <++> whitespace1 <++> ("-" <$ (string "-" <|> numberSymbol))
                 <++> whitespace <++> number
          var2 = key <++> whitespace <++> essenceNameByWordsComplex key stop

essenceNameWithKeyRight :: Parser String -> Parser a -> Parser String
essenceNameWithKeyRight key stop = var1 <|> var2
    where var1 = essenceNameByWords key stop <++> whitespace1 <++> key
                 <++> whitespace <++> numberSymbol <++> whitespace <++> number
          var2 = essenceNameByWordsComplex key stop <++> whitespace1 <++> key

essenceNameWithKey :: Parser String -> Parser a -> Parser String
essenceNameWithKey key stop = essenceNameWithKeyLeft key stop
                              <|> essenceNameWithKeyRight key stop

essenceNameWithNumeric :: Parser String -> Parser a -> Parser String
essenceNameWithNumeric key stop = var1 <|> var2
    where var1 = numerical <++> whitespace1 <++> essenceNameByWordsComplex key stop
          var2 = numerical <++> whitespace1 <++> streetKey

essenceNameWithNumericAndKey :: Parser String -> Parser a -> Parser String
essenceNameWithNumericAndKey key stop = var1 <|> var2 <|> var3 <|> var4
    where var1 = numerical <++> whitespace1 <++> essenceNameByWordsComplex key stop
                 <++> whitespace1 <++> key
          var2 = numerical <++> whitespace <++> key
                 <++> whitespace <++> essenceNameByWordsComplex key stop
          var3 = key <++> whitespace <++> numerical
                 <++> whitespace <++> essenceNameByWordsComplex key stop
          var4 = essenceNameByWordsComplex key stop
                 <++> whitespace <++> numerical
                 <++> whitespace <++> key

streetNameWithKey :: Parser String
streetNameWithKey = var1 <|> var2 <|> var3 <|> kremlin
    where var1 = essenceNameWithKey streetKey houseNumber
          var2 = (microDistrict <|> projectedPassage <|> passage <|> area)
                 <++> whitespace <++> numberSymbol <++> whitespace <++> number
          var3 = microDistrict <++> whitespace <++> (toList <$> ruLetter) <* endOfRuWord

streetNameWithSeveralKeys :: Parser String
streetNameWithSeveralKeys = varQuayStreet
                            <|> varStreetPlus
                            <|> varAreaPlus
                            <|> varSizedBoulevar
                            <|> varSizedNamedStreet
    where varQuayStreet = option ((big <|> small) <++> whitespace)
                          <++> quay <++> whitespace <++> street
          varStreetPlus = street <++> whitespace
                          <++> option (essenceNameByWordsComplex streetKey houseNumber)
                          <++> whitespace1 <++> (alley <|> area <|> bridge
                                                 <|> settlement <|> avenue)
          varAreaPlus = area
                        <++> whitespace <++> essenceNameByWordsComplex streetKey houseNumber
                        <++> whitespace1 <++> boulevar
          varSizedBoulevar = (big <|> small) <++> whitespace <++> boulevar
          varSizedNamedStreet = (big <|> small)
                                <++> whitespace <++> essenceNameByWordsComplex streetKey houseNumber
                                <++> whitespace1 <++> streetKey

railway :: Parser String
railway = ((string "железной" <++> whitespace1 <++> string "дороги")
          <|> (string "железная" <++> whitespace1 <++> string "дорога"))
          <* endOfEssenceKey

railwayShortening :: Parser String
railwayShortening = (many (checkNot (string "ЖД") *> ruLetter) <++> string "ЖД")
                    <* endOfEssenceKey

autoRoad :: Parser String
autoRoad = (string "автодороги" <|> string "автодорога") <* endOfEssenceKey

autoRoadShortening :: Parser String
autoRoadShortening = (many (checkNot (string "АД") *> ruLetter) <++> string "АД")
                     <* endOfEssenceKey

kilometreNumber :: Parser String
kilometreNumber = numerical <++> whitespace1 <++> kilometre

streetNameWithKilometre :: Parser String
streetNameWithKilometre = var1 <|> var2 <|> var3 <|> var4 <|> var5
                          <|> var6 <|> var7 <|> var8 <|> var9 <|> var10
    where var1  = essenceNameByWords key houseNumber <++> whitespace1 <++> railway
                  <++> sep <++> essenceNameByWords key houseNumber
                  <++> sep <++> kilometreNumber
          var2  = essenceNameByWords key houseNumber <++> whitespace1 <++> railway
                  <++> sep <++> kilometreNumber
          var3  = railwayShortening <++> sep <++> essenceNameByWords key houseNumber
                  <++> sep <++> kilometreNumber
          var4  = railwayShortening <++> sep <++> kilometreNumber
          var5  = kilometreNumber <++> whitespace <++> essenceNameByWords key houseNumber
                  <++> whitespace1 <++> railway
          var6  = kilometreNumber <++> whitespace <++> railwayShortening
          var7  = kilometreNumber <++> whitespace <++> essenceNameByWords key houseNumber
                  <++> whitespace1 <++> autoRoad
          var8  = kilometreNumber <++> whitespace <++> autoRoadShortening
          var9  = autoRoadShortening <++> sep <++> kilometreNumber
          var10 = essenceNameWithKey key houseNumber <++> sep <++> kilometreNumber
          sep   = ", " <$ (comma <|> whitespace1)
          key   = streetKey <|> railwayShortening <|> autoRoadShortening

streetNameExotic :: Parser String
streetNameExotic = var1 <|> var2
    where var1 = (streetNameWithSeveralKeys <|> streetNameWithKey)
                 <++> essenceSeparator <++> olimpicVillage
          var2 = streetKey <++> whitespace <++> essenceNameByWords key houseNumber
                 <++> whitespace1 <++> railway
          key   = streetKey

streetNameStrictedByKey :: Parser String
streetNameStrictedByKey = streetNameExotic
             <|> streetNameWithKilometre
             <|> essenceNameWithNumericAndKey key houseNumber
             <|> streetNameWithSeveralKeys
             <|> streetNameWithKey
    where key = streetKey

streetName :: Parser String
streetName = streetNameStrictedByKey
             <|> essenceNameWithNumeric key houseNumber
             <|> essenceNameByWordsComplex key houseNumber
    where key = streetKey

town :: Parser String
town = ("город" <$ (string "город" <|> string "гор" <|> string "г")) <* endOfEssenceKey

territory :: Parser String
territory = ("территория" <$ (string "территория" <|> string "тер")) <* endOfEssenceKey

innerTownTerritory :: Parser String
innerTownTerritory = (("внутригородская" <$ (string "внутригородская"
                                             <|> string "внутригор"))
                      <* endOfEssenceKey)
                     <++> whitespace <++> territory

okrug :: Parser String
okrug = ("округ" <$ (string "округ" <|> string "окр")) <* endOfEssenceKey

municipalOkrug :: Parser String
municipalOkrug = (("муниципальный" <$ (string "муниципальный" <|> string "муниц"))
                  <* endOfEssenceKey)
                 <++> whitespace <++> okrug

urbanOkrug :: Parser String
urbanOkrug = (("городской" <$ (string "городской" <|> string "гор" <|> string "г"))
                  <* endOfEssenceKey)
                 <++> whitespace <++> okrug

colony :: Parser String
colony = ("поселение" <$ (string "поселение" <|> string "посел" <|> string "п"))
         <* endOfEssenceKey

settlement :: Parser String
settlement = ("посёлок" <$ (string "поселок" <|> string "посёлок" <|> string "пос"))
             <* endOfEssenceKey

labourSettlement :: Parser String
labourSettlement = (("рабочий" <$ (string "рабочий" <|> string "раб"))
                    <* endOfEssenceKey)
                   <++> whitespace <++> settlement

dachaSettlement :: Parser String
dachaSettlement = (("дачный" <$ (string "дачный" <|> string "дачн" <|> string "дач"))
                   <* endOfEssenceKey)
                  <++> whitespace <++> settlement

residentialSettlement :: Parser String
residentialSettlement = (("жилой" <$ (string "жилой" <|> string "жил" <|> string "ж"))
                         <* endOfEssenceKey)
                        <++> whitespace <++> settlement

district :: Parser String
district = (("район" <$ (string "район" <|> string "р-н")) <|> string "районе")
           <* endOfEssenceKey

selo :: Parser String
selo = ("село" <$ (string "село" <|> string "сел")) <* endOfEssenceKey

village :: Parser String
village = ("деревня" <$ (string "деревня" <|> string "дер")) <* endOfEssenceKey

olimpicVillage :: Parser String
olimpicVillage = string "олимпийская" <++> whitespace1 <++> village

subtownKey :: Parser String
subtownKey = innerTownTerritory
             <|> autonomusOkrug
             <|> territory
             <|> metroTown
             <|> town
             <|> municipalOkrug
             <|> urbanOkrug
             <|> okrug
             <|> colony
             <|> labourSettlement
             <|> dachaSettlement
             <|> residentialSettlement
             <|> settlement
             <|> district
             <|> selo
             <|> village
             <|> olimpicVillage

commonKey :: Parser String
commonKey = subtownKey <|> streetKey <|> subcountryKey <|> russia

essenceSeparator :: Parser String
essenceSeparator = ", " <$ (comma
                            <|> (whitespace <* check (commonKey <++> whitespace))
                            <|> (whitespace <* check numerical)
                            <|> (whitespace <* check number))

anySubtownName :: Parser String -> Parser String
anySubtownName key = essenceNameWithNumericAndKey key houseNumber
                     <|> essenceNameWithKeyLeft key houseNumber

autonomusOkrug :: Parser String
autonomusOkrug = shots
                 <|> (directions <++> whitespace1 <++> aoFull)
                 <|> aoFull
    where aoFull = "автономный округ" <$ ((autonomus <++> whitespace <++> okrug) <|> ao)
          ao = string "ао" <* endOfEssenceKey
          autonomus = ("автономный" <$ (string "автономный" <|> string "авт"))
                      <* endOfEssenceKey
          directions = (string "Центральный"
                        <|> string "Северный"
                        <|> (string "Северо" <++> dash <++> string "Восточный")
                        <|> string "Восточный"
                        <|> (string "Юго"    <++> dash <++> string "Восточный")
                        <|> string "южный"
                        <|> (string "Юго"    <++> dash <++> string "Западный")
                        <|> string "Западный"
                        <|> (string "Северо" <++> dash <++> string "Западный"))
                       <* endOfEssenceKey
          dash = "-" <$ (string "-" <|> whitespace)
          shots = (string "цао"
                   <|> ("Северный автономный округ"         <$ string "сао")
                   <|> ("Северо-Восточный автономный округ" <$ string "свао")
                   <|> ("Восточный автономный округ"        <$ string "вао")
                   <|> ("Юго-Восточный автономный округ"    <$ string "ювао")
                   <|> ("Южный автономный округ"            <$ string "юао")
                   <|> ("Юго-Западный автономный округ"     <$ string "юзао")
                   <|> ("Запандый автономный округ"         <$ string "зао")
                   <|> ("Северо-Западный автономный округ"  <$ string "сзао"))
                  <* endOfEssenceKey

subtownName :: Parser String
subtownName = innerTownTerritory
              <|> autonomusOkrug
              <|> metroTown
              <|> anySubtownName key
    where key = territory
                <|> municipalOkrug
                <|> urbanOkrug
                <|> okrug
                <|> town
                <|> colony
                <|> district
                <|> selo
                <|> village

anyOkrugName :: Parser String -> Parser String
anyOkrugName okrugKey = (okrugKey <++> whitespace <++> (metroTown <|> nameLikeStreet))
            <|> anySubtownName okrugKey
    where nameLikeStreet = streetNameStrictedByKey
                           <* check (comma
                                     <|> (essenceSeparator *> streetNameStrictedByKey))

okrugName :: Parser String
okrugName = anyOkrugName okrug

municipalOkrugName :: Parser String
municipalOkrugName = anyOkrugName municipalOkrug

urbanOkrugName :: Parser String
urbanOkrugName = anyOkrugName urbanOkrug

settlementName :: Parser String
settlementName = essenceNameWithNumericAndKey key houseNumber
                 <|> essenceNameWithKey key houseNumber
                 <|> numeredOnly
    where numeredOnly = key <++> whitespace <++> numberSymbol
                        <++> whitespace <++> number
          key = labourSettlement
                <|> dachaSettlement
                <|> residentialSettlement
                <|> settlement

districtName :: Parser String
districtName = essenceNameWithNumericAndKey key houseNumber
               <|> essenceNameWithKey key houseNumber
    where key = district

realTownName :: Parser String
realTownName = anySubtownName town <|> nameWithoutKey
    where nameWithoutKey = checkNot (streetNameWithKilometre
                                     <|> essenceNameWithNumericAndKey streetKey houseNumber
                                     <|> houseNumber)
                           *> essenceNameByWordsComplex town houseNumber
                           <* endOfTownNameWithoutKey

subtownsNames :: Parser [String]
subtownsNames = (subtownName <|> settlementName) `sepBy` essenceSeparator

data Subtown = InnerTownTerritory String
             | AutonomusOkrug     String
             | RealTown           String
             | Territory          String
             | MunicipalOkrug     String
             | UrbanOkrug         String
             | Okrug              String
             | Colony             String
             | Settlement         String
             | District           String
             | Selo               String
             | Village            String
             deriving Eq

instance Show Subtown where
    show (InnerTownTerritory text) = "innerTownTerritory='" ++ text ++ "'"
    show (AutonomusOkrug text)     = "autonomusOkrug='"     ++ text ++ "'"
    show (RealTown text)           = "town='"               ++ text ++ "'"
    show (Territory text)          = "territory='"          ++ text ++ "'"
    show (MunicipalOkrug text)     = "municipalOkrug='"     ++ text ++ "'"
    show (UrbanOkrug text)         = "urbanOkrug='"         ++ text ++ "'"
    show (Okrug text)              = "okrug='"              ++ text ++ "'"
    show (Colony text)             = "colony='"             ++ text ++ "'"
    show (Settlement text)         = "settlement='"         ++ text ++ "'"
    show (District text)           = "district='"           ++ text ++ "'"
    show (Selo text)               = "selo='"               ++ text ++ "'"
    show (Village text)            = "village='"            ++ text ++ "'"

listToStr :: Show a => [a] -> String
listToStr = intercalate ", " . map show

innerTownTerritoryNameType :: Parser Subtown
innerTownTerritoryNameType = InnerTownTerritory <$> innerTownTerritory

autonomusOkrugNamyType :: Parser Subtown
autonomusOkrugNamyType = AutonomusOkrug <$> autonomusOkrug

metroTownNameType :: Parser Subtown
metroTownNameType = RealTown <$> metroTown

endOfTownNameWithoutKey :: Parser String
endOfTownNameWithoutKey = check (end1 <|> end2 <|> end3 <|> end4)
                          <* checkNot negativEnd
    where end1 = essenceSeparator <* ("" <$ subtownsNames)
          end2 = essenceSeparator <* (essenceNameWithKeyLeft streetKey houseNumber
                                      <|> streetNameWithKilometre
                                      <|> streetNameExotic
                                      <|> essenceNameWithNumericAndKey streetKey houseNumber
                                      <|> essenceNameWithNumeric streetKey houseNumber
                                      <|> streetNameWithSeveralKeys)
          end3 = comma <* streetNameWithKey
          end4 = comma <* essenceNameByWordsComplex streetKey houseNumber
          negativEnd = essenceSeparator <* houseNumber

realTownNameType :: Parser Subtown
realTownNameType = RealTown <$> realTownName

territoryNameType :: Parser Subtown
territoryNameType = Territory <$> anySubtownName territory

municipalOkrugNameType :: Parser Subtown
municipalOkrugNameType = MunicipalOkrug <$> municipalOkrugName

urbanOkrugNameType :: Parser Subtown
urbanOkrugNameType = UrbanOkrug <$> urbanOkrugName

okrugNameType :: Parser Subtown
okrugNameType = Okrug <$> okrugName

colonyNameType :: Parser Subtown
colonyNameType = Colony <$> anySubtownName colony

settlementNameType :: Parser Subtown
settlementNameType = Settlement <$> settlementName

districtNameType :: Parser Subtown
districtNameType = District <$> districtName

seloNameType :: Parser Subtown
seloNameType = Selo <$> anySubtownName selo

villageNameType :: Parser Subtown
villageNameType = Village <$> anySubtownName village

subtownsNamesTypes :: Parser [Subtown]
subtownsNamesTypes = name `sepBy` essenceSeparator
    where name = innerTownTerritoryNameType
                 <|> autonomusOkrugNamyType
                 <|> metroTownNameType
                 <|> territoryNameType
                 <|> municipalOkrugNameType
                 <|> urbanOkrugNameType
                 <|> okrugNameType
                 <|> realTownNameType
                 <|> colonyNameType
                 <|> settlementNameType
                 <|> districtNameType
                 <|> seloNameType
                 <|> villageNameType

region :: Parser String
region = ("область" <$ (string "область" <|> string "обл")) <* endOfEssenceKey

republic :: Parser String
republic = ("республика" <$ (string "республика" <|> string "рес")) <* endOfEssenceKey

subcountryKey :: Parser String
subcountryKey = republic <|> region

townName :: Parser String
townName = realTownName

regionName :: Parser String
regionName = anySubcounryName region

republicName :: Parser String
republicName = anySubcounryName republic

anySubcounryName :: Parser String -> Parser String
anySubcounryName = flip essenceNameWithKey halt

data Subcountry = Republic String
                | Region String
                deriving Eq

instance Show Subcountry where
    show (Republic text) = "republic='" ++ text ++ "'"
    show (Region text)   = "region='"   ++ text ++ "'"

regionNameType :: Parser Subcountry
regionNameType = Region <$> regionName

republicNameType :: Parser Subcountry
republicNameType = Republic <$> republicName

subcountriesNamesTypes :: Parser [Subcountry]
subcountriesNamesTypes = name `sepBy` essenceSeparator
    where name = republicNameType
                 <|> regionNameType

russia :: Parser String
russia = ("Российская Федерация" <$ ((string "российская" <++> whitespace1
                                      <++> string "федерация")
                                     <|> string "рф"
                                     <|> string "россия"))
         <* endOfEssenceKey

countryName :: Parser String
countryName = russia -- <|> essenceNameByWords commonKey
