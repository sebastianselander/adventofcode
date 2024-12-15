{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Advent.Format where

import Control.Arrow ((>>>))
import Control.Monad (forM, replicateM, void, (<=<))
import Data.Char ( digitToInt, isUpper, isSpace )
import Data.Data (Data, Typeable)
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List (foldl', isPrefixOf, sortBy, stripPrefix)
import Language.Haskell.TH (
    Con (NormalC),
    Dec (DataD),
    ExpQ,
    Info (TyConI),
    Name,
    Q,
    Type (AppT, ConT, ListT, TupleT),
    TypeQ,
    conE,
    conT,
    listE,
    lookupTypeName,
    mkName,
    nameBase,
    reify,
    tupleDataName,
    varE,
 )
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.Directory.Extra (doesFileExist)
import Text.Parsec (
    ParseError,
    Parsec,
    anyChar,
    between,
    chainl1,
    char,
    choice,
    digit,
    eof,
    getInput,
    letter,
    many,
    many1,
    newline,
    satisfy,
    noneOf,
    oneOf,
    option,
    optionMaybe,
    optional,
    parse,
    sepBy,
    string,
    try,
    (<?>),
    (<|>),
 )
import Text.Parsec.Expr (
    Assoc (..),
    Operator (..),
    OperatorTable,
    buildExpressionParser,
 )
import Text.Printf (printf)
import Data.List.Extra (splitOn)
import Advent.Coord (coordArray, Coord)
import Data.Array.Base (IArray)

intro :: Q [Dec]
intro = return []

data Format
    = Empty
    | Signed
    | Digit
    | Unsigned
    | Char
    | Newline
    | Symbol
    | String
    | Optional !Format
    | At !String
    | Literal !String
    | Gather !Format
    | Group !Format
    | Many !Format
    | Some !Format
    | Discard !Format
    | Alternative !Format !Format
    | SepBy !Format !Format
    | Follows !Format !Format
    deriving (Show, Typeable, Data)

getTestInput :: Int -> IO String
getTestInput year = do
    let file = "inputs/%d/test.txt"
    executable <- doesFileExist file
    if executable
        then readFile (printf file year)
        else -- hack work around for repl
            readFile (printf ("/home/sebastian/Documents/git/adventofcode/" <> file) year)

getRawInput :: Int -> Int -> IO String
getRawInput year day = do
    let file = "inputs/%d/%02d.txt"
    executable <- doesFileExist file
    if executable
        then readFile (printf file year day)
        else -- hack work around for repl
            readFile (printf ("/home/sebastian/Documents/git/adventofcode/" <> file) year day)

getArrayInput :: IArray a Char => Int -> Int -> IO (a Coord Char)
getArrayInput year day = coordArray . lines <$> getRawInput year day

{- |
%i - parse an integer, optionally prefixed by `+` or `-`
%u - parse any unsigned integer, not prefixed by +
%d - parse any single digit
%n - parse a newline
%c - parse any lower-case, upper-case or
     title-case unicode plus letters according to isAlpha
%s - like char, but for strings
%y - parse one of `!@#$%^&*_+=|'\`
<fmt>! - save the result of as a string <fmt>
<fmt>? - zero or one
<fmt>* - zero or many
<fmt>+ - one or many
~<fmt> - discard the result
<fmt1>|<fmt2> - <fmt1> or <fmt2>
<fmt1>&<fmt2>- zero or more <fmt1> separated by <fmt1>
For example in `data Foo = Foo_LT` it will then create a parse usable by `@Foo` that parses `<` and saves it as `Foo_LT`
This only works for the following symbols.
Parsing an arbitrary text can be done as follows: `Foo_bar`. This creates a parser that parses `bar` and returns the constructor `Foo_bar` in its place.
LT: <
GT: >
EQ: =
BANG: !
AT: @
HASH: #
DOLLAR: $
PERCENT: %
CARET: ^
AMPERSAND: &
STAR: *
PIPE: |
LPAREN: (
RPAREN: )
LBRACE: {
RBRACE: }
LBRACK: [
RBRACK: ]
COLON: :
SEMI: ;
QUESTION: ?
SLASH: /
BACKSLASH: \\
UNDERSCORE: _
DASH: -
DOT: .
COMMA: :
PLUS: +
TILDE: ~
-}
format' :: QuasiQuoter
format' =
    QuasiQuoter
        { quoteExp = makeParser <=< parseFormat
        , quoteType = toType <=< (fmap (\(_, _, p) -> p) . parseFormat)
        , quotePat = const $ fail "Patterns not supported"
        , quoteDec = const $ fail "Decs not supported"
        }
  where
    makeParser (year, _, p) =
        [|
            let fmtparser = parseErr ($(toParser p) <* eof)
             in fmtparser <$> getTestInput year
            |]

format :: QuasiQuoter
format =
    QuasiQuoter
        { quoteExp = makeParser <=< parseFormat
        , quoteType = toType <=< (fmap (\(_, _, p) -> p) . parseFormat)
        , quotePat = const $ fail "Patterns not supported"
        , quoteDec = const $ fail "Decs not supported"
        }
  where
    makeParser (year, day, p) =
        [|
            let fmtparser = parseErr ($(toParser p) <* eof)
             in fmtparser <$> getRawInput year day
            |]

{- |
%i - parse an integer, optionally prefixed by `+` or `-`
%u - parse any unsigned integeger, not prefixed by +
%d - parse any single digit
%n - parse a newline
%c - parse any lower-case, upper-case or
     title-case unicode plus letters according to isAlpha
%s - like char, but for strings
%y - parse one of `!@#$%^&*_+=|'\`
<fmt>! - save the result of as a string <fmt>
<fmt>? - zero or one
<fmt>* - zero or many
<fmt>+ - one or many
~<fmt> - discard the result
<fmt1>|<fmt2> - <fmt1> or <fmt2>
<fmt1>&<fmt2>- zero or more <fmt1> separated by <fmt1>
For example in `data Foo = Foo_LT` it will then create a parse usable by `@Foo` that parses `<` and saves it as `Foo_LT`
This only works for the following symbols.
Parsing an arbitrary text can be done as follows: `Foo_bar`. This creates a parser that parses `bar` and returns the constructor `Foo_bar` in its place.
LT: <
GT: >
EQ: =
BANG: !
AT: @
HASH: #
DOLLAR: $
PERCENT: %
CARET: ^
AMPERSAND: &
STAR: *
PIPE: |
LPAREN: (
RPAREN: )
LBRACE: {
RBRACE: }
LBRACK: [
RBRACK: ]
COLON: :
SEMI: ;
QUESTION: ?
SLASH: /
BACKSLASH: \\
UNDERSCORE: _
DASH: -
DOT: .
COMMA: :
PLUS: +
TILDE: ~
-}
fmt :: QuasiQuoter
fmt =
    QuasiQuoter
        { quoteExp = makeParser <=< parseFmt
        , quoteType = toType <=< parseFmt
        , quotePat = const $ fail "Patterns not supported"
        , quoteDec = const $ fail "Decs not supported"
        }
  where
    makeParser p = [|let fmtparser = parseErr ($(toParser p) <* eof) in fmtparser|]

toType :: Format -> TypeQ
toType = \case
    Empty -> [t|()|]
    Newline -> [t|()|]
    Unsigned -> [t|Int|]
    Symbol -> [t|Char|]
    Signed -> [t|Int|]
    Digit -> [t|Int|]
    String -> [t|String|]
    Char -> [t|Char|]
    Literal _ -> [t|()|]
    Discard _ -> [t|()|]
    Optional format
        | interesting format -> [t|Maybe $(toType format)|]
        | otherwise -> [t|()|]
    At [] -> fail "At: no text"
    At tag@(t : _)
        | isUpper t -> conT (mkName tag)
        | otherwise -> fail "toType: can't read type variables"
    Group format -> [t|$(toType format)|]
    Gather format -> [t|$(toType format)|]
    Many format ->
        if interesting format
            then do
                ty <- [t|[$(toType format)]|]
                if isCharTy ty then [t|String|] else pure ty
            else [t|()|]
    Some format ->
        if interesting format
            then do
                ty <- [t|[$(toType format)]|]
                if isCharTy ty then [t|String|] else pure ty
            else [t|()|]
    SepBy format _ ->
        if interesting format
            then do
                ty <- [t|[$(toType format)]|]
                if isCharTy ty then [t|String|] else pure ty
            else [t|()|]
    Alternative l r
        | interesting l, interesting r -> [t|Either $(toType l) $(toType r)|]
        | interesting l -> [t|Maybe $lt|]
        | interesting r -> [t|Maybe $rt|]
        | otherwise -> [t|()|]
      where
        lt = toType l
        rt = toType r
    Follows l r -> [t|($(toType l), $(toType r))|]

toParser :: Format -> ExpQ
toParser = \case
    Empty -> [|return () <?> "<empty>"|]
    Newline -> [|void newline <?> "newline"|]
    String -> [|many1 (satisfy (not . isSpace)) <?> "<letters>"|]
    Symbol -> [|many1 symbol <?> "<symbol>"|]
    Unsigned -> [|unsigned <?> "unsigned"|]
    Signed -> [|signed <?> "signed"|]
    Digit -> [|digitToInt <$> digit <?> "digit"|]
    Char -> [|letter <?> "<single letter>"|]
    Discard format -> [|$(toParser format) $> ()|]
    Optional format
        | interesting format -> [|option Nothing (Just <$> $(toParser format))|]
        | otherwise -> [|optional $(toParser format)|]
    Gather format -> [|fst <$> gather $(toParser format)|]
    At [] -> fail "At: empty string"
    At tag@(t : _)
        | isUpper t -> makeEnumParser tag
        | otherwise -> varE (mkName tag)
    Literal str -> [|void (string str)|]
    Group format -> [|$(toParser format)|]
    Many format ->
        if interesting format
            then [|many $(toParser format)|]
            else [|void (many $(toParser format))|]
    Some format ->
        if interesting format
            then [|many1 $(toParser format)|]
            else [|void (many1 $(toParser format))|]
    SepBy l r ->
        if interesting l
            then [|sepBy $(toParser l) $(toParser r)|]
            else [|void (sepBy $(toParser l) $(toParser r))|]
    Alternative l r
        | interesting l, interesting r -> [|Left <$> try $le <|> Right <$> $re|]
        | interesting l -> [|Just <$> try $le <|> Nothing <$ $re|]
        | interesting r -> [|Nothing <$ try $le <|> Just <$> $re|]
        | otherwise -> [|$le <|> $re|]
      where
        le = toParser l
        re = toParser r
    format@(Follows _ _) -> do
        let fmts = [(interesting x, toParser x) | x <- flatten format []]
            n = foldl' (\acc (x, _) -> if x then acc + 1 else acc) 0 fmts
            tup = conE (tupleDataName n)
        case fmts of
            [] -> [|return ()|]
            ((ii, e) : es)
                | n == 0 -> foldl' ap0 e es
                | n == 1 -> foldl' ap1 e es
                | ii -> foldl' apN [|$tup <$> $e|] es
                | otherwise -> foldl' apN [|$tup <$ $e|] es
      where
        ap0 l (_, r) = [|$l *> $r|]
        ap1 l (i, r) = if i then [|$l *> $r|] else [|$l <* $r|]
        apN l (i, r) = if i then [|$l <*> $r|] else [|$l <* $r|]

interesting :: Format -> Bool
interesting = \case
    Empty -> False
    Literal{} -> False
    Newline -> False
    Signed -> True
    Digit -> True
    Symbol -> True
    Unsigned -> True
    Char -> True
    At _ -> True
    String -> True
    Gather _ -> True
    Discard _ -> False
    Optional format -> interesting format
    Group format -> interesting format
    Many format -> interesting format
    Some format -> interesting format
    SepBy l _ -> interesting l
    Alternative l r -> interesting l || interesting r
    Follows l r -> interesting l || interesting r

gather :: Parser a -> Parser (String, a)
gather p = do
    before <- getInput
    res <- p
    afterLen <- length <$> getInput
    let parStr = take (length before - afterLen) before
    return (parStr, res)

symbol :: Parser Char
symbol = oneOf ".!@#$%^&*_+=|'\";:"

decimal :: [Int] -> Int
decimal = foldl' (\acc -> ((10 * acc) +)) 0

unsigned :: Parser Int
unsigned = decimal <$> many1 (digitToInt <$> digit)

signed :: Parser Int
signed = do
    f <- option id (char '-' $> negate <|> char '+' $> id)
    f <$> unsigned

flatten :: Format -> [Format] -> [Format]
flatten (Literal x) (Literal y : ys) = flatten (Literal (x ++ y)) ys
flatten Empty xs = xs
flatten (Follows l r) xs = flatten l (flatten r xs)
flatten x ys = x : ys

parseFormat :: String -> Q (Int, Int, Format)
parseFormat s = case parseEither ((,,) <$> year <*> day <*> factor1 <* eof) s of
    Left err -> fail (show err)
    Right r -> return r
  where
    year = decimal <$> replicateM 4 (digitToInt <$> digit) <* (char ' ' <?> "exactly one space")
    day = f <$> digit <*> optionMaybe digit <* (char ' ' <?> "exactly one space")
      where
        f x Nothing = digitToInt x
        f x (Just y) = 10 * digitToInt x + digitToInt y

parseFmt :: String -> Q Format
parseFmt s = case parseEither (factor1 <* eof) s of
    Left err -> fail (show err)
    Right r -> return r

orderByLongest :: [(a, Either Format String)] -> [(a, Either Format String)]
orderByLongest = sortBy (prefixOrder `on` snd)
  where
    prefixOrder :: Either Format String -> Either Format String -> Ordering
    prefixOrder (Left _) (Left _) = EQ
    prefixOrder (Left _) (Right _) = GT
    prefixOrder (Right _) (Left _) = LT
    prefixOrder (Right l) (Right r)
        | l `isPrefixOf` r = GT
        | r `isPrefixOf` l = LT
        | otherwise = EQ

makeEnumParser :: String -> ExpQ
makeEnumParser xs = do
    cases <- orderByLongest <$> go xs
    let parsers =
            [ case str of
                Right str' -> [|$(conE name) <$ try (string str')|]
                Left fmt' -> [|$(conE name) <$> try $(toParser fmt')|]
            | (name, str) <- cases
            ]
    [|choice $(listE parsers)|]
  where
    go :: String -> Q [(Name, Either Format String)]
    go str = do
        tyInfo <- lookupTypeName str
        tyName <- maybe (fail "Data type not found") return tyInfo
        cons <-
            reify tyName >>= \case
                TyConI (DataD _ _ _ _ cons _) -> return cons
                _ -> fail $ "Failed finding data type: " <> str
        forM cons $ \case
            NormalC n []
                | Just name <- stripPrefix str (nameBase n) ->
                    case name of
                        '_' : symbolName -> do
                            sym <- processSymbolName symbolName
                            return (n, pure sym)
                        _ -> fail "Constructor name must be separated by '_'"
                | otherwise ->
                    fail $
                        "Constructor '"
                            <> nameBase n
                            <> "' must be prefixed by its type name as follows: '"
                            <> nameBase tyName
                            <> "_"
                            <> nameBase n
                            <> "'"
            NormalC n [(_, ty)]
                | Just name <- stripPrefix str (nameBase n) ->
                    case name of
                        '_' : _ -> do
                            fmt' <- tyToFormat ty
                            return (n, Left fmt')
                        _ -> fail "Constructor must be be separated by '_'"
            con -> fail $ "Not an enum or one argument constructor: " <> show con

tyToFormat :: Type -> Q Format
tyToFormat ty = case ty of
    ConT name -> case nameBase name of
        "Nat" -> pure Unsigned
        "Int" -> pure Signed
        "String" -> pure String
        "Char" -> pure Char
        otherName -> fail $ "Type not supported: " <> otherName
    Maybe x -> Optional <$> tyToFormat x
    Either l r -> Alternative <$> tyToFormat l <*> tyToFormat r
    Tuple l r -> do
        l' <- tyToFormat l
        r' <- tyToFormat r
        pure $ Follows (Follows l' (Many (Literal " "))) r'
    List x -> do
        x' <- tyToFormat x
        pure $ Many (Follows x' (Many (Literal " ")))
    NonEmpty x -> do
        x' <- tyToFormat x
        pure $ Some (Follows x' (Many (Literal " ")))
    _ -> fail $ "tyToFormat not implemented for: " <> show ty

pattern NonEmpty :: Type -> Type
pattern NonEmpty x <- AppT (ConT (nameBase -> "NonEmpty")) x

pattern List :: Type -> Type
pattern List x <- AppT ListT x

pattern Tuple :: Type -> Type -> Type
pattern Tuple l r <- AppT (AppT (TupleT 2) l) r

pattern Maybe :: Type -> Type
pattern Maybe x <- AppT (ConT (nameBase -> "Maybe")) x

pattern Either :: Type -> Type -> Type
pattern Either l r <- AppT (AppT (ConT (nameBase -> "Either")) l) r


isCharTy :: Type -> Bool
isCharTy (ConT (nameBase -> "Char")) = True
isCharTy _ = False

processSymbolName :: String -> Q String
processSymbolName str = pure $ concatMap (\x -> maybe x (:"") (lookup x symbolNames)) xs
  where
    xs = splitOn "_" str

    -- case break ('_' ==) str of
    --     (name, rest) ->
    --         case lookup name symbolNames of
    --             Nothing -> case rest of
    --                 [] -> pure name
    --                 _ -> (name <>) <$> processSymbolName rest
    --             Just sym ->
    --                 case rest of
    --                     [] -> pure [sym]
    --                     _ : str' -> (sym :) <$> processSymbolName str'

symbolNames :: [(String, Char)]
symbolNames =
    [ ("LT", '<')
    , ("GT", '>')
    , ("EQ", '=')
    , ("BANG", '!')
    , ("AT", '@')
    , ("HASH", '#')
    , ("DOLLAR", '$')
    , ("PERCENT", '%')
    , ("CARET", '^')
    , ("AMPERSAND", '&')
    , ("STAR", '*')
    , ("PIPE", '|')
    , ("LPAREN", '(')
    , ("RPAREN", ')')
    , ("LBRACE", '{')
    , ("RBRACE", '}')
    , ("LBRACK", '[')
    , ("RBRACK", ']')
    , ("COLON", ':')
    , ("SEMI", ';')
    , ("QUESTION", '?')
    , ("SLASH", '/')
    , ("BACKSLASH", '\\')
    , ("UNDERSCORE", '_')
    , ("DASH", '-')
    , ("DOT", '.')
    , ("COMMA", ',')
    , ("PLUS", '+')
    , ("TILDE", '~')
    ]

i :: Parser Int
i = signed

u :: Parser Int
u = unsigned

d :: Parser Int
d = digitToInt <$> digit

n :: Parser ()
n = void newline

c :: Parser Char
c = letter

s :: Parser String
s = many1 letter

y :: Parser Char
y = symbol

-- Parsing for the Quasi Quoter

type Parser = Parsec String ()
type Table a = OperatorTable String () Identity a

parseEither :: Parser a -> String -> Either ParseError a
parseEither p = parse (p <* eof) ""

parseErr :: Parser a -> String -> a
parseErr p s = case parseEither p s of
    Left err -> error (show err)
    Right r -> r

factor1 :: Parser Format
factor1 =
    choice
        [ try $ factor2 `chainl1` (char '|' $> Alternative)
        , factor2
        ]
factor2 :: Parser Format
factor2 =
    choice
        [ try $ factor3 `chainl1` (return () $> Follows)
        , factor3
        ]

factor3 :: Parser Format
factor3 =
    choice
        [ try $ buildExpressionParser table atom
        , atom
        ]

atom :: Parser Format
atom =
    choice
        [ Group <$> between (char '(') (char ')') factor1
        , foldl1 Alternative
            <$> between
                (char '[')
                (char ']')
                (many1 (Literal <$> fmap (: []) (chars <|> char '\\' *> anyChar)))
        , char '%'
            *> choice
                [ char 's' $> String
                , char 'i' $> Signed
                , char 'c' $> Char
                , char 'd' $> Digit
                , char 'n' $> Newline
                , char 'u' $> Unsigned
                , char 'y' $> Symbol
                ]
        , char '@' *> (At <$> many1 letter)
        , Literal <$> (char '\\' *> fmap (: []) anyChar)
        , Literal <$> many1 chars
        ]
chars :: Parser Char
chars = noneOf "%()\\*+&|@!?[]"

table :: Table Format
table =
    [
        [ Prefix $ foldr1 (>>>) <$> many1 (char '~' $> Discard)
        ]
    ,
        [ Postfix $
            foldr1 (>>>)
                <$> many1
                    ( choice
                        [ char '!' $> Gather
                        , char '?' $> Optional
                        , char '*' $> Many
                        , char '+' $> Some
                        ]
                    )
        ]
    ,
        [ Infix (char '&' $> SepBy) AssocLeft
        ]
    ]
