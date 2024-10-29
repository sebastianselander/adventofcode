{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Example where

import Format

data Color = Color_Red | Color_Green | Color_Blue
    deriving Show

intro

-- parse csv where v : Int
parseCsv :: String -> [Int]
parseCsv = [fmt|( *%d( )*)&,|]



parseColor :: String -> Color
parseColor = [fmt|@Color|]
