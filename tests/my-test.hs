import Data.Maybe
import Test.HUnit

import Parser
import AddressParser.Internal
import AddressParser

main :: IO Int
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then return 1 else return 0

unitTest :: String -> Assertion -> Test
unitTest name = TestLabel name . TestCase

tests = TestList [
    unitTest "anyChar ABC" $
        assertEqual "" (Right ("BC", 'A')) $
        parse anyChar "ABC"

    , unitTest "char A ABC" $
        assertEqual "" (Right ("BC", 'A')) $
        parse (char 'A') "ABC"

    , unitTest "char B ABC" $
        assertEqual "" (Left "unexpected A") $
        parse (char 'B') "ABC"

    , unitTest "many char A" $
        assertEqual "" (Right ("BC", "AAA")) $
        parse (many (char 'A')) "AAABC"

    , unitTest "string Hello" $
        assertEqual "" (Right (", world!", "Hello")) $
        parse (string "Hello") "Hello, world!"

    , unitTest "string 'AAA'" $
        assertEqual "" (Right ("", "AAA")) $
        parse (string "AAA") "aaa"

    , unitTest "string' 'AAA'" $
        assertEqual "" (Left "unexpected AAA") $
        parse (string' "AAA") "aaa"

    , unitTest "many string Hello" $
        assertEqual "" (Right (", world!", ["Hello", "Hello", "Hello"])) $
        parse (many (string "Hello"))  "HelloHelloHello, world!"

    , unitTest "optionMaybe number 123k1" $
        assertEqual "" (Right ("k1", Just "123")) $
        parse (optionMaybe number) "123k1"

    , unitTest "option 'hello, world'" $
        assertEqual "" (Right (", world", "hello")) $
        parse (option (string "hello")) "hello, world"

    , unitTest "option 'good day, world'" $
        assertEqual "" (Right ("good day, world", "")) $
        parse (option (string "hello")) "good day, world"

    , unitTest "number 123k1" $
        assertEqual "" (Right ("k1", "123")) $
        parse number "123k1"

    , unitTest "sepBy '123, 35, 6789' ', '" $
        assertEqual "" (Right ("", ["123", "35", "6789"])) $
        parse (number `sepBy` (string ", " *> many (char ' '))) "123, 35, 6789"

    , unitTest "sepBy '123' ', '" $
        assertEqual "" (Right ("", ["123"])) $
        parse (number `sepBy` (string ", " *> many (char ' '))) "123"

    , unitTest "check 'one two three'" $
        assertEqual "" (Right ("two three", "two")) $
        parse (string "one " *> check (string "two")) "one two three"

    , unitTest "check 'one nonTwo three'" $
        assertEqual "" (Left "unexpected two") $
        parse (string "one " *> check (string "two")) "one nonTwo three"

    , unitTest "checkNot 'one two three'" $
        assertEqual "" (Left "checking detected") $
        parse (string "one " *> checkNot (string "two")) "one two three"

    , unitTest "checkNot 'nonOne two three'" $
        assertEqual "" (Right (" two three", "nonOne")) $
        parse (checkNot (string "one") *> string "nonOne") "nonOne two three"

    , unitTest "complexBlockNumber 123/1б-456б/н OTHERS" $
        assertEqual "" (Right (" OTHERS", "123/1б-456б/н")) $
        parse complexBlockNumber "123/1б-456б/н OTHERS"

    , unitTest "complexBlockNumber б/н" $
        assertEqual "" (Right ("", "б/н")) $
        parse complexBlockNumber "б/н"

    , unitTest "complexBlockNumber 66г-д" $
        assertEqual "" (Right ("", "66г-д")) $
        parse complexBlockNumber "66г-д"

    , unitTest "complexBlockNumber 66/77" $
        assertEqual "" (Right ("", "66/77")) $
        parse complexBlockNumber "66/77"

    , unitTest "complexBlockNumber 42аб" $
        assertEqual "" (Right ("", "42аб")) $
        parse complexBlockNumber "42аб"

    , unitTest "complexBlockNumber г, " $
        assertEqual "" (Right (", ", "г")) $
        parse complexBlockNumber "г, "

    , unitTest "complexBlockNumber блок,аб" $
        assertEqual "" (Right (",аб", "блок")) $
        parse complexBlockNumber "блок,аб"

    , unitTest "complexBlockNumber б-в" $
        assertEqual "" (Right ("", "б-в")) $
        parse complexBlockNumber "б-в"

    , unitTest "complexBlockNumber б-в.г" $
        assertEqual "" (Right (".г", "б-в")) $
        parse complexBlockNumber "б-в.г"

    , unitTest "complexBlockNumber 123-д" $
        assertEqual "" (Right ("", "123-д")) $
        parse complexBlockNumber "123-д"

    , unitTest "complexBlockNumber 4-76а-2004" $
        assertEqual "" (Right ("", "4-76а-2004")) $
        parse complexBlockNumber "4-76а-2004"

    , unitTest "complexBlockNumber 8д/14/61" $
        assertEqual "" (Right ("", "8д/14/61")) $
        parse complexBlockNumber "8д/14/61"

    , unitTest "complexBlockNumber 8д/14/61, " $
        assertEqual "" (Right (", ", "8д/14/61")) $
        parse complexBlockNumber "8д/14/61, "

    , unitTest "endOfHouseWord ''" $
        assertEqual "" (Right ("", "")) $
        parse endOfHouseWord ""

    , unitTest "endOfHouseWord ', 18а'" $
        assertEqual "" (Right (", 18а", ",")) $
        parse endOfHouseWord ", 18а"

    , unitTest "endOfHouseWord ' 18а'" $
        assertEqual "" (Right (" 18а", " ")) $
        parse endOfHouseWord " 18а"

    , unitTest "endOfHouseWord '18а'" $
        assertEqual "" (Right ("18а", "1")) $
        parse endOfHouseWord "18а"

    , unitTest "endOfHouseWord 'я8а'" $
        assertEqual "" (Left "unexpected end of house word") $
        parse endOfHouseWord "я18а"

    , unitTest "houseKey 'дом1'" $
        assertEqual "" (Right ("1", "дом")) $
        parse houseKey "дом1"

    , unitTest "houseNumber 'вл. 12 корп. 98'" $
        assertEqual "" (Right ("", "владение 12, корпус 98")) $
        parse houseNumber "вл. 12 корп. 98"

    , unitTest "houseNumber 'вл. 3/81'" $
        assertEqual "" (Right ("", "владение 3/81")) $
        parse houseNumber "вл. 3/81"

    , unitTest "houseNumber 'владение 1, корпус 2, строение 3 and something'" $
        assertEqual "" (Right (" and something", "владение 1, корпус 2, строение 3")) $
        parse houseNumber "владение 1, корпус 2, строение 3 and something"

    , unitTest "houseNumber 'дом 1, корпус 23г-д, строение 45'" $
        assertEqual "" (Right ("", "дом 1, корпус 23г-д, строение 45")) $
        parse houseNumber "дом 1, корпус 23г-д, строение 45"

    , unitTest "houseNumber '1, корпус 23г-д, строение 45'" $
        assertEqual "" (Right ("", "дом 1, корпус 23г-д, строение 45")) $
        parse houseNumber "1, корпус 23г-д, строение 45"

    , unitTest "houseNumber '1с2'" $
        assertEqual "" (Right ("", "дом 1, строение 2")) $
        parse houseNumber "1с2"

    , unitTest "houseNumber '66к2'" $
        assertEqual "" (Right ("", "дом 66, корпус 2")) $
        parse houseNumber "66к2"

    , unitTest "houseNumber 'дом 1,   корпус зона, строение в-2-3-4'" $
        assertEqual "" (Right ("", "дом 1, корпус зона, строение в-2-3-4")) $
        parse houseNumber "дом 1,   корпус зона, строение в-2-3-4"

    , unitTest "houseNumber '1корпус зона   строение в-2-3-4'" $
        assertEqual "" (Right ("", "дом 1, корпус зона, строение в-2-3-4")) $
        parse houseNumber "1корпус зона   строение в-2-3-4"

    , unitTest "houseNumber 'дом 12а/34, строение 56а'" $
        assertEqual "" (Right ("", "дом 12а/34, строение 56а")) $
        parse houseNumber "дом 12а/34, строение 56а"

    , unitTest "houseNumber 'земельный участок 87/5'" $
        assertEqual "" (Right ("", "земельный участок 87/5")) $
        parse houseNumber "земельный участок 87/5"

    , unitTest "ruWord1 ''" $
        assertEqual "" (Left "unexpected end of input") $
        parse ruWord1 ""

    , unitTest "ruWord1 '()'" $
        assertEqual "" (Left "unexpected (") $
        parse ruWord1 "("

    , unitTest "complexRuWord 'тверская-ямская-продольно-поперечная и что-то'" $
        assertEqual "" (Right (" и что-то", "тверская-ямская-продольно-поперечная")) $
        parse complexRuWord "тверская-ямская-продольно-поперечная и что-то"

    , unitTest "complexRuWord 'гоголя'" $
        assertEqual "" (Right ("", "гоголя")) $
        parse complexRuWord "гоголя"

    , unitTest "complexRuWord1 ''" $
        assertEqual "" (Left "unexpected end of input") $
        parse complexRuWord1 ""

    , unitTest "complexRuWord1 '()'" $
        assertEqual "" (Left "unexpected (") $
        parse complexRuWord1 "()"

    , unitTest "complexRuWord '()'" $
        assertEqual "" (Right ("()", "")) $
        parse complexRuWord "()"

    , unitTest "essenceNameByWords 'некая такая улица'" $
        assertEqual "" (Right (" улица", "некая такая")) $
        parse (essenceNameByWords streetKey halt) "некая такая улица"

    , unitTest "essenceNameByWordsComplex 'фабрики имени 1 мая'" $
        assertEqual "" (Right ("", "фабрики имени 1 мая")) $
        parse (essenceNameByWordsComplex streetKey halt) "фабрики имени 1 мая"

    , unitTest "essenceNameByWordsComplex 'некий'" $
        assertEqual "" (Right ("", "некий")) $
        parse (essenceNameByWordsComplex streetKey halt) "некий"

    , unitTest "essenceNameByWordsComplex 'название 26-ти бакинских комиссаров'" $
        assertEqual "" (Right ("", "название 26-ти бакинских комиссаров")) $
        parse (essenceNameByWordsComplex streetKey halt) "название 26-ти бакинских комиссаров"

    , unitTest "essenceNameByWordsComplex 'мосрентген 2-й лялькин 11'" $
        assertEqual "" (Right (" 2-й лялькин 11", "мосрентген")) $
        parse (essenceNameByWordsComplex town halt) "мосрентген 2-й лялькин 11"

    , unitTest "essenceNameWithKey 'некий проезд'" $
        assertEqual "" (Right ("", "некий проезд")) $
        parse (essenceNameWithKey streetKey halt) "некий проезд"

    , unitTest "essenceNameWithKey 'некий проезд № 7 and something else'" $
        assertEqual "" (Right (" and something else", "некий проезд № 7")) $
        parse (essenceNameWithKey streetKey halt) "некий проезд № 7 and something else"

    , unitTest "essenceNameWithKey 'некий    проезд №7 and something else'" $
        assertEqual "" (Right (" and something else", "некий проезд № 7")) $
        parse (essenceNameWithKey streetKey halt) "некий    проезд №7 and something else"

    , unitTest "streetNameWithKey 'микрорайон № 1'" $
        assertEqual "" (Right ("", "микрорайон № 1")) $
        parse streetNameWithKey "микрорайон № 1"

    , unitTest "streetNameWithSeveralKeys 'б. набережная улица'" $
        assertEqual "" (Right ("", "бол. набережная улица")) $
        parse streetNameWithSeveralKeys "б. набережная улица"

    , unitTest "streetNameWithSeveralKeys 'малая набережная улица крякозяб'" $
        assertEqual "" (Right (" крякозяб", "малая набережная улица")) $
        parse streetNameWithSeveralKeys "малая набережная улица крякозяб"

    , unitTest "streetNameWithSeveralKeys 'набережная улица'" $
        assertEqual "" (Right ("", "набережная улица")) $
        parse streetNameWithSeveralKeys "набережная улица"

    , unitTest "streetNameWithSeveralKeys 'улица садовый квартал'" $
        assertEqual "" (Right ("", "улица садовый квартал")) $
        parse streetNameWithSeveralKeys "улица садовый квартал"

    , unitTest "streetNameWithKilometre 'малое кольцо московской железной дороги, 54-й километр'" $
        assertEqual "" (Right ("", "малое кольцо московской железной дороги, 54-й километр")) $
        parse streetNameWithKilometre "малое кольцо московской железной дороги 54-й километр"

    , unitTest "streetNameWithKilometre 'малое кольцо московской железной дороги, 34-й км'" $
        assertEqual "" (Right ("", "малое кольцо московской железной дороги, 34-й километр")) $
        parse streetNameWithKilometre "малое кольцо московской железной дороги, 34-й км"

    , unitTest "streetNameWithKilometre 'московская железная дорога, курское, 18-ый километр'" $
        assertEqual "" (Right ("", "московская железная дорога, курское, 18-й километр")) $
        parse streetNameWithKilometre "московская железная дорога, курское, 18-ый километр"

    , unitTest "streetNameWithKilometre 'октябрьская железная дорога, москва-санкт-петербург, 10-ый километр'" $
        assertEqual "" (Right ("", "октябрьская железная дорога, москва-санкт-петербург, 10-й километр")) $
        parse streetNameWithKilometre "октябрьская железная дорога, москва-санкт-петербург, 10-ый километр"

    , unitTest "streetNameWithKilometre 'мжд, киевское, 2-ой километр'" $
        assertEqual "" (Right ("", "мЖД, киевское, 2-й километр")) $
        parse streetNameWithKilometre "мжд, киевское, 2-ой километр"

    , unitTest "streetNameWithKilometre '19-й километр московской кольцевой автодороги'" $
        assertEqual "" (Right ("", "19-й километр московской кольцевой автодороги")) $
        parse streetNameWithKilometre "19-й километр московской кольцевой автодороги"

    , unitTest "streetNameWithKilometre '19-й километр мкад'" $
        assertEqual "" (Right ("", "19-й километр мкАД")) $
        parse streetNameWithKilometre "19-й километр мкад"

    , unitTest "streetNameWithKilometre 'малое кольцо московской железной дороги, 54-й километр'" $
        assertEqual "" (Right ("", "малое кольцо московской железной дороги, 54-й километр")) $
        parse streetNameWithKilometre "малое кольцо московской железной дороги, 54-й километр"

    , unitTest "streetNameWithKilometre 'калужское шоссе, 20-й километр'" $
        assertEqual "" (Right ("", "калужское шоссе, 20-й километр")) $
        parse streetNameWithKilometre "калужское шоссе, 20-й километр"

    , unitTest "streetNameWithKilometre 'мкад, 47-й километр нечто'" $
        assertEqual "" (Right (" нечто", "мкАД, 47-й километр")) $
        parse streetNameWithKilometre "мкад, 47-й километр нечто"

    , unitTest "streetName 'ул.алексеевский пр-т'" $
        assertEqual "" (Right ("", "улица алексеевский проспект")) $
        parse streetName "улица алексеевский проспект"

    , unitTest "streetName 'улица рогожский посёлок'" $
        assertEqual "" (Right ("", "улица рогожский посёлок")) $
        parse streetName "улица рогожский посёлок"

    , unitTest "essenceSeparator ' , что-то написано'" $
        assertEqual "" (Right ("что-то написано", ", ")) $
        parse essenceSeparator " , что-то написано"

    , unitTest "essenceSeparator '  поселение загорское'" $
        assertEqual "" (Right ("поселение загорское", ", ")) $
        parse essenceSeparator "  поселение загорское"

    , unitTest "essenceSeparator 'деревня поречье'" $
        assertEqual "" (Right ("деревня поречье", ", ")) $
        parse essenceSeparator "деревня поречье"

    , unitTest "essenceSeparator ',дачный поселок ромашка'" $
        assertEqual "" (Right ("дачный поселок ромашка", ", ")) $
        parse essenceSeparator ",дачный поселок ромашка"

    , unitTest "essenceSeparator 'улица уличная'" $
        assertEqual "" (Right ("улица уличная", ", ")) $
        parse essenceSeparator "улица уличная"

    , unitTest "essenceSeparator '2-ой лялькин'" $
        assertEqual "" (Right ("2-ой лялькин", ", ")) $
        parse essenceSeparator "2-ой лялькин"

    , unitTest "subtownName 'поселение сосенское'" $
        assertEqual "" (Right ("", "поселение сосенское")) $
        parse subtownName "поселение сосенское"

    , unitTest "subtownName 'территория инновационного центра \"сколково\"'" $
        assertEqual "" (Right ("", "территория инновационного центра \"сколково\"")) $
        parse subtownName "территория инновационного центра \"сколково\""

    , unitTest "subtownName 'округ марьина роща, московская железная дорога'" $
        assertEqual "" (Right (", московская железная дорога", "округ марьина роща")) $
        parse subtownName "округ марьина роща, московская железная дорога"

    , unitTest "settlementName 'посёлок дорожно-ремонтного пункта - 3 1'" $
        assertEqual "" (Right (" 1", "посёлок дорожно-ремонтного пункта - 3")) $
        parse settlementName "посёлок дорожно-ремонтного пункта - 3 1"

    , unitTest "settlementName 'посёлок дорожно-ремонтного пункта # 3'" $
        assertEqual "" (Right ("", "посёлок дорожно-ремонтного пункта - 3")) $
        parse settlementName "посёлок дорожно-ремонтного пункта # 3"

    , unitTest "subtownsNames 'поселение сосенское'" $
        assertEqual "" (Right ("", ["поселение сосенское"])) $
        parse subtownsNames "поселение сосенское"

    , unitTest "subtownsNames 'муниципальный округ марьина роща'" $
        assertEqual "" (Right ("", ["муниципальный округ марьина роща"])) $
        parse subtownsNames "муниципальный округ марьина роща"

    , unitTest "subtownsNames 'внутригородская территория муниципальный округ марьина роща'" $
        assertEqual "" (Right ("", [ "внутригородская территория"
                                   , "муниципальный округ марьина роща"])) $
        parse subtownsNames "внутригородская территория муниципальный округ марьина роща"

    , unitTest "subtownsNames 'внутригородская территория, муниципальный округ марьина роща'" $
        assertEqual "" (Right ("", [ "внутригородская территория"
                                   , "муниципальный округ марьина роща"])) $
        parse subtownsNames "внутригородская территория, муниципальный округ марьина роща"

    , unitTest "subtownsNames 'внутригородская территория, муниципальный округ марьина роща, что-то еще'" $
        assertEqual "" (Right (", что-то еще", [ "внутригородская территория"
                                               , "муниципальный округ марьина роща"])) $
        parse subtownsNames "внутригородская территория, муниципальный округ марьина роща, что-то еще"

    , unitTest "subtownsNames 'внутригородская территория муниципальный округ марьина роща улица уличная'" $
        assertEqual "" (Right (" улица уличная", [ "внутригородская территория"
                                                 , "муниципальный округ марьина роща"])) $
        parse subtownsNames "внутригородская территория муниципальный округ марьина роща улица уличная"

    , unitTest "subtownsNames 'внутригородская территория муниципальный округ марьина роща, уличная улица'" $
        assertEqual "" (Right (", уличная улица", [ "внутригородская территория"
                                                  , "муниципальный округ марьина роща"])) $
        parse subtownsNames "внутригородская территория муниципальный округ марьина роща, уличная улица"

    , unitTest "subtownsNames 'округ марьина роща, московская железная дорога 77'" $
        assertEqual "" (Right ( ", московская железная дорога 77"
                              , ["округ марьина роща"])) $
        parse subtownsNames "округ марьина роща, московская железная дорога 77"

    , unitTest "subtownsNamesTypes 'внутригородская территория муниципальный округ марьина роща, улица уличная'" $
        assertEqual "" (Right (", улица уличная", [ InnerTownTerritory "внутригородская территория"
                                                 , MunicipalOkrug "муниципальный округ марьина роща"])) $
        parse subtownsNamesTypes "внутригородская территория муниципальный округ марьина роща, улица уличная"

    , unitTest "subtownsNamesTypes 'внутригородская территория муниципальный округ марьина роща, уличная улица'" $
        assertEqual "" (Right (", уличная улица", [ InnerTownTerritory "внутригородская территория"
                                                  , MunicipalOkrug "муниципальный округ марьина роща"])) $
        parse subtownsNamesTypes "внутригородская территория муниципальный округ марьина роща, уличная улица"

    , unitTest "essenceNameWithKey 'город зеленоград'" $
        assertEqual "" (Right ("", "город зеленоград")) $
        parse (essenceNameWithKey town halt) "город зеленоград"

    , unitTest "essenceNameByWordsComplex 'михайлово, квартал № 98'" $
        assertEqual "" (Right (", квартал № 98", "михайлово")) $
        parse (essenceNameByWordsComplex town halt <* endOfTownNameWithoutKey) "михайлово, квартал № 98"

    , unitTest "realTownName 'михайлово, квартал № 98'" $
        assertEqual "" (Right (", квартал № 98", "михайлово")) $
        parse realTownName "михайлово, квартал № 98"

    , unitTest "subtownsNamesTypes 'город зеленоград'" $
        assertEqual "" (Right ("", [RealTown "город зеленоград"])) $
        parse subtownsNamesTypes "город зеленоград"

    , unitTest "subtownsNamesTypes 'город зеленоград, улица басманная'" $
        assertEqual "" (Right (", улица басманная", [RealTown "город зеленоград"])) $
        parse subtownsNamesTypes "город зеленоград, улица басманная"

    , unitTest "regionName 'область тверская, калязинский район'" $
        assertEqual "" (Right (", калязинский район", "область тверская")) $
        parse regionName "область тверская, калязинский район"

    , unitTest "addressDataHouseInt ['владение 26','корпус 6']" $
        assertEqual "" (Just 26) $
        addressDataHouseInt $ AddressData Nothing Nothing Nothing Nothing Nothing
                                          (Just ["владение 26","корпус 6"])

    , unitTest "addressDataHouseInt ['владение а','корпус б/н']" $
        assertEqual "" Nothing $
        addressDataHouseInt $ AddressData Nothing Nothing Nothing Nothing Nothing
                                          (Just ["владение а","корпус б/н"])

    , unitTest "addressDataHouseInt []" $
        assertEqual "" Nothing $
        addressDataHouseInt $ AddressData Nothing Nothing Nothing Nothing Nothing
                                          (Just [])

    , unitTest "addressDataHouseInt ['владение а','корпус 3/4']" $
        assertEqual "" (Just 3) $
        addressDataHouseInt $ AddressData Nothing Nothing Nothing Nothing Nothing
                                          (Just ["владение а","корпус 3/4"])

    , unitTest "address 'некий проезд № 7 1" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "некий проезд № 7")
                                  (Just ["дом 1"]))) $
        parse address "некий проезд № 7 1"

    , unitTest "address 'нек. проезд № 7 1" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "нек. проезд № 7")
                                  (Just ["дом 1"]))) $
        parse address "нек. проезд № 7 1"

    , unitTest "address 'некий проезд № 7 1 and something else" $
        assertEqual "" (Right ( " and something else"
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "некий проезд № 7")
                                  (Just ["дом 1"]))) $
        parse address "некий проезд № 7 1 and something else"

    , unitTest "address 'некий проезд № 7, дом 1 and something else'" $
        assertEqual "" (Right ( " and something else"
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "некий проезд № 7")
                                  (Just ["дом 1"]))) $
        parse address "некий проезд № 7, дом 1 and something else"

    , unitTest "address 'некий проезд № 7, 1с4 and something else'" $
        assertEqual "" (Right ( " and something else"
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "некий проезд № 7")
                                  (Just ["дом 1", "строение 4"]))) $
        parse address "некий проезд № 7, 1с4 and something else"

    , unitTest "address 'некий проезд № 7, 1, строение 4 and something else'" $
        assertEqual "" (Right ( " and something else"
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "некий проезд № 7")
                                  (Just ["дом 1", "строение 4"]))) $
        parse address "некий проезд № 7, 1, строение 4 and something else"

    , unitTest "address 'город зеленоград, проезд № 5526'" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing
                                  (Just "город зеленоград")
                                  Nothing
                                  (Just "проезд № 5526")
                                  Nothing)) $
        parse address "город зеленоград, проезд № 5526"

    , unitTest "address 'город москва, 104-й километр московской кольцевой автодороги вл1'" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing
                                  (Just "город москва")
                                  Nothing
                                  (Just "104-й километр московской кольцевой автодороги")
                                  (Just ["владение 1"]))) $
        parse address "город москва, 104-й километр московской кольцевой автодороги вл1"

    , unitTest "address 'октябрьская железная дорога москва-санкт-петербург 10-й километр вл3 с4 собачья конура'" $
        assertEqual "" (Right ( " собачья конура"
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "октябрьская железная дорога, москва-санкт-петербург, 10-й километр")
                                  (Just ["владение 3", "строение 4"]))) $
        parse address "октябрьская железная дорога москва-санкт-петербург 10-й километр вл3 с4 собачья конура"

    , unitTest "address 'город москва, городской округ троицк, город троицк, 1-я изумрудная улица'" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing
                                  (Just "город москва")
                                  (Just [ UrbanOkrug "городской округ троицк"
                                        , RealTown "город троицк"])
                                  (Just "1-я изумрудная улица")
                                  Nothing)) $
        parse address "город москва, городской округ троицк, город троицк, 1-я изумрудная улица"

    , unitTest "address 'г. москва, кв-л. 3-й капотня, д. 16, кв. 45'" $
        assertEqual "" (Right ( ", кв. 45"
                              , AddressData Nothing Nothing
                                  (Just "город москва")
                                  Nothing
                                  (Just "квартал 3-й капотня")
                                  (Just ["дом 16"]))) $
        parse address "г. москва, кв-л. 3-й капотня, д. 16, кв. 45"

    , unitTest "address 'город москва, внутригородская территория муниципальный округ матушкино, город зеленоград, проспект генерала алексеева 8с2'" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing
                                  (Just "город москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ матушкино"
                                        , RealTown "город зеленоград"])
                                  (Just "проспект генерала алексеева")
                                  (Just ["дом 8", "строение 2"]))) $
        parse address "город москва, внутригородская территория муниципальный округ матушкино, город зеленоград, проспект генерала алексеева 8с2"

    , unitTest "address 'город москва внутригородская территория муниципальный округ матушкино город зеленоград проспект генерала алексеева 8с2'" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing
                                  (Just "город москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ матушкино"
                                        , RealTown "город зеленоград"])
                                  (Just "проспект генерала алексеева")
                                  (Just ["дом 8", "строение 2"]))) $
        parse address "город москва, внутригородская территория муниципальный округ матушкино, город зеленоград, проспект генерала алексеева 8с2"

    , unitTest "address 'мжд киевское 2-й километр вл8'" $
        assertEqual "" (Right ( ""
                              , AddressData Nothing Nothing Nothing Nothing
                                  (Just "мЖД, киевское, 2-й километр")
                                  (Just ["владение 8"]))) $
        parse address "мжд киевское 2-й километр вл8"

    , unitTest "address 'рф город москва внутригородская территория поселение рязановское посёлок фабрики имени 1 мая микрорайон гора 2'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , Colony "поселение рязановское"
                                        , Settlement "посёлок фабрики имени 1 мая"])
                                  (Just "микрорайон гора")
                                  (Just ["дом 2"]))) $
        parse address "рф город москва внутригородская территория поселение рязановское посёлок фабрики имени 1 мая микрорайон гора 2"

    , unitTest "address 'РФ Город Москва Внутригородская Территория Поселение Рязановское Посёлок Фабрики Имени 1 Мая Микрорайон Гора 2'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , Colony "поселение Рязановское"
                                        , Settlement "посёлок Фабрики имени 1 Мая"])
                                  (Just "микрорайон Гора")
                                  (Just ["дом 2"]))) $
        parse address "РФ Город Москва Внутригородская Территория Поселение Рязановское Посёлок Фабрики Имени 1 Мая Микрорайон Гора 2"

    , unitTest "address 'Российская Федерация город Москва внутригородская территория муниципальный округ Силино город Зеленоград улица Гоголя земельный участок 1013А'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ Силино"
                                        , RealTown "город Зеленоград"])
                                  (Just "улица Гоголя")
                                  (Just ["земельный участок 1013А"]))) $
        parse address "Российская Федерация город Москва внутригородская территория муниципальный округ Силино город Зеленоград улица Гоголя земельный участок 1013А"

    , unitTest "address 'Российская Федерация город Москва внутригородская территория муниципальный округ Проспект Вернадского проспект Вернадского дом 76 корпус Е строение 2'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ проспект Вернадского"])
                                  (Just "проспект Вернадского")
                                  (Just ["дом 76","корпус Е","строение 2"]))) $
        parse address "Российская Федерация город Москва внутригородская территория муниципальный округ Проспект Вернадского проспект Вернадского дом 76 корпус Е строение 2"

    , unitTest "address 'Москва ВАО Метрогородок Открытое шоссе вл. 26 корп. 6'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  Nothing
                                  Nothing
                                  (Just "Москва")
                                  (Just [ AutonomusOkrug "Восточный автономный округ"
                                        , RealTown "Метрогородок"])
                                  (Just "Открытое шоссе")
                                  (Just ["владение 26","корпус 6"]))) $
        parse address "Москва ВАО Метрогородок Открытое шоссе вл. 26 корп. 6"

    , unitTest "address 'Российская Федерация город Москва внутригородская территория поселение Рязановское территория СНТ \"Ветеран-2\" земельный участок 66'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , Colony "поселение Рязановское"
                                        , Territory "территория СНТ \"Ветеран-2\""])
                                  Nothing
                                  (Just ["земельный участок 66"]))) $
        parse address "Российская Федерация город Москва внутригородская территория поселение Рязановское территория СНТ \"Ветеран-2\" земельный участок 66"

    , unitTest "address 'Российская Федерация город Москва внутригородская территория муниципальный округ Тимирязевский улица Линии Октябрьской Железной Дороги дом 10'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ Тимирязевский"])
                                  (Just "улица Линии Октябрьской железной дороги")
                                  (Just ["дом 10"]))) $
        parse address "Российская Федерация город Москва внутригородская территория муниципальный округ Тимирязевский улица Линии Октябрьской Железной Дороги дом 10"

    , unitTest "address 'Российская Федерация город Москва внутригородская территория муниципальный округ Лефортово, Левый тупик дом 5/7'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ Лефортово"])
                                  (Just "Левый тупик")
                                  (Just ["дом 5/7"]))) $
        parse address "Российская Федерация город Москва внутригородская территория муниципальный округ Лефортово, Левый тупик дом 5/7"

    , unitTest "address 'Российская Федерация город Москва внутригородская территория муниципальный округ Косино-Ухтомский улица 3-го Интернационала дом 10Б'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ Косино-Ухтомский"])
                                  (Just "улица 3-го Интернационала")
                                  (Just ["дом 10Б"]))) $
        parse address "Российская Федерация город Москва внутригородская территория муниципальный округ Косино-Ухтомский улица 3-го Интернационала дом 10Б"

    , unitTest "address 'Российская Федерация город Москва внутригородская территория муниципальный округ Арбат улица Малая Молчановка дом 2'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  (Just "Российская Федерация")
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ InnerTownTerritory "внутригородская территория"
                                        , MunicipalOkrug "муниципальный округ Арбат"])
                                  (Just "улица Малая Молчановка")
                                  (Just ["дом 2"]))) $
        parse address "Российская Федерация город Москва внутригородская территория муниципальный округ Арбат улица Малая Молчановка дом 2"

    , unitTest "address 'город Москва, город Троицк, микрорайон \"В\", дом 10'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  Nothing
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ RealTown "город Троицк"])
                                  (Just "микрорайон \"В\"")
                                  (Just ["дом 10"]))) $
        parse address "город Москва, город Троицк, микрорайон \"В\", дом 10"

    , unitTest "address 'город Москва, город Троицк, микрорайон В, дом 10'" $
        assertEqual "" (Right ( ""
                              , AddressData
                                  Nothing
                                  Nothing
                                  (Just "город Москва")
                                  (Just [ RealTown "город Троицк"])
                                  (Just "микрорайон В")
                                  (Just ["дом 10"]))) $
        parse address "город Москва, город Троицк, микрорайон В, дом 10"

    , unitTest "rests '2-я улица собачьей конуры, дом 1, владение 16б'" $
        assertEqual "" [ "2-я улица собачьей конуры, дом 1, владение 16б"
                       , "улица собачьей конуры, дом 1, владение 16б"
                       , "собачьей конуры, дом 1, владение 16б"
                       , "конуры, дом 1, владение 16б"
                       , "дом 1, владение 16б"
                       , "1, владение 16б"
                       , "владение 16б"
                       , "16б"] $
        rests "2-я улица собачьей конуры, дом 1, владение 16б"

    , unitTest "rests ', 2-я улица собачьей конуры, дом 1, владение 16б'" $
        assertEqual "" [ "2-я улица собачьей конуры, дом 1, владение 16б"
                       , "улица собачьей конуры, дом 1, владение 16б"
                       , "собачьей конуры, дом 1, владение 16б"
                       , "конуры, дом 1, владение 16б"
                       , "дом 1, владение 16б"
                       , "1, владение 16б"
                       , "владение 16б"
                       , "16б"] $
        rests ", 2-я улица собачьей конуры, дом 1, владение 16б"
    ]
