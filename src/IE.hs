{-# LANGUAGE OverloadedStrings #-}

module IE ( jsonDumps
          , jsonDumpsPretty
          , jsonDumpsFull
          , jsonDumpsFullPretty
          , showAddressesJson
          , showOneAddressJson
          , showOneAddressJsonPretty
          , showBestAddressJson
          , showBestAddressJsonPretty
          ) where

import Data.List (intercalate)
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import Data.Aeson
import Data.Aeson.Encode.Pretty

import AddressParser.Internal ( Subtown (..)
                              , Subcountry (..))
import AddressParser ( AddressData(..)
                     , adderessDataRealTown
                     , addressDataHouseInt
                     , selectAddress, selectBestAddress
                     )

jsonDumps :: ToJSON a => a -> String
jsonDumps = T.unpack . TE.decodeUtf8 . encode

jsonDumpsPretty :: ToJSON a => a -> String
jsonDumpsPretty = T.unpack . TE.decodeUtf8 . encodePretty

jsonDumpsFull :: String -> [AddressData] -> String
jsonDumpsFull text variants = jsonDumps $ FullAnswer text variants

jsonDumpsFullPretty :: String -> [AddressData] -> String
jsonDumpsFullPretty text variants = jsonDumpsPretty $ FullAnswer text variants

showAddressesJson :: ToJSON a => [a] -> String
showAddressesJson addresses = intercalate "\n"
                              $ zipWith (curry showPair) [1..]
                              $ map jsonDumps addresses
    where showPair p = show (fst p) ++ ". " ++ snd p

showOneAddressJson :: [AddressData] -> String
showOneAddressJson [] = "{}"
showOneAddressJson xs = jsonDumps $ selectAddress xs

showOneAddressJsonPretty :: [AddressData] -> String
showOneAddressJsonPretty [] = "{}"
showOneAddressJsonPretty xs = jsonDumpsPretty $ selectAddress xs

showBestAddressJson :: [AddressData] -> String
showBestAddressJson [] = "{}"
showBestAddressJson xs = jsonDumps $ selectBestAddress xs

showBestAddressJsonPretty :: [AddressData] -> String
showBestAddressJsonPretty [] = "{}"
showBestAddressJsonPretty xs = jsonDumpsPretty $ selectBestAddress xs

instance ToJSON Subtown where
    toJSON (InnerTownTerritory text) = object [ "name"  .= ("innerTownTerritory" :: String)
                                              , "value" .= text ]
    toJSON (AutonomusOkrug text)     = object [ "name"  .= ("autonomusOkrug" :: String)
                                              , "value" .= text ]
    toJSON (RealTown text)           = object [ "name"  .= ("town" :: String)
                                              , "value" .= text ]
    toJSON (Territory text)          = object [ "name"  .= ("territory" :: String)
                                              , "value" .= text ]
    toJSON (MunicipalOkrug text)     = object [ "name"  .= ("municipalOkrug" :: String)
                                              , "value" .= text ]
    toJSON (UrbanOkrug text)         = object [ "name"  .= ("urbanOkrug" :: String)
                                              , "value" .= text ]
    toJSON (Okrug text)              = object [ "name"  .= ("okrug" :: String)
                                              , "value" .= text ]
    toJSON (Colony text)             = object [ "name"  .= ("colony" :: String)
                                              , "value" .= text ]
    toJSON (Settlement text)         = object [ "name"  .= ("settlement" :: String)
                                              , "value" .= text ]
    toJSON (District text)           = object [ "name"  .= ("district" :: String)
                                              , "value" .= text ]
    toJSON (Selo text)               = object [ "name"  .= ("selo" :: String)
                                              , "value" .= text ]
    toJSON (Village text)            = object [ "name"  .= ("village" :: String)
                                              , "value" .= text ]
instance ToJSON Subcountry where
    toJSON (Republic text) = object [ "name"  .= ("republic" :: String)
                                    , "value" .= text ]
    toJSON (Region text)   = object [ "name"  .= ("region" :: String)
                                    , "value" .= text ]

instance ToJSON AddressData where
    toJSON ad = object $ filter ((/= Null) . snd)
                [ "country"    .= addressDataCountry     ad
                , "subcountry" .= addressDataSubcountry  ad
                , "town"       .= addressDataTown        ad
                , "subtown"    .= addressDataSubtown     ad
                , "realTown"   .= adderessDataRealTown   ad
                , "street"     .= addressDataStreet      ad
                , "house"      .= addressDataHouse       ad
                , "houseInt"   .= addressDataHouseInt    ad
                ]

data FullAnswer = FullAnswer String [AddressData]

instance ToJSON FullAnswer where
    toJSON (FullAnswer text []) = object [ "raw"       .= text
                                         , "_variants" .= ([] :: [AddressData])
                                         , "address"   .= object []
                                         ]
    toJSON (FullAnswer text xs) = object [ "raw"       .= text
                                         , "_variants" .= xs
                                         , "address"   .= selectAddress xs
                                         ]
