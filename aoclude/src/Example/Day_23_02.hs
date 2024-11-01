{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Example.Day_23_02 where

import Format

data Color = Color_red | Color_green | Color_blue
    deriving Show

intro

parse :: String -> [(Int, [[(Int, Color)]])]
parse = [fmt|(Game %i: ((%i @Color)&(, )&(; ))%n)*|]

input :: String
input = 
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 11: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"
