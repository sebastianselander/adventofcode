{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Advent.ReadS where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Control.Monad (ap, liftM)
import Data.Functor (void)
import Data.String (IsString (..))

-- | Wrapper for 'ReadS'
newtype P a = P {unP :: ReadS a}

-- | Parse a string or throw an error
runP :: P a -> String -> a
runP (P f) (f -> (x, _) : _) = x
runP _ x = error ("failed to parse: " ++ x)

-- | Match a specific string token and return it.
tok :: String -> P String
tok t = do u <- P lex; if t == u then pure u else empty

-- | Match a leading character
char :: Char -> P ()
char c = P \case
    x : xs | c == x -> [((), xs)]
    _ -> []

instance Functor P where
    fmap = liftM

instance Applicative P where
    (<*>) = ap
    pure x = P \s -> [(x, s)]

instance Monad P where
    P m >>= f = P \s -> do (x, s') <- m s; case f x of P g -> g s'

instance Alternative P where
    P x <|> P y = P \s -> x s <|> y s
    empty = P (const [])

instance MonadFail P where
    fail _ = empty

-- | String literals match with 'tok'
instance (a ~ String) => IsString (P a) where
    fromString = tok

-- * Combinators

-- | Parse a separated, nonempty list.
sepBy1 ::
    -- | element
    P a ->
    -- | separator
    P b ->
    P [a]
sepBy1 p q = (:) <$> p <*> many (q *> p)

-- | Parse a separated list.
sepBy ::
    -- | element
    P a ->
    -- | separator
    P b ->
    P [a]
sepBy p q = pure [] <|> sepBy1 p q

-- | Convenience function for surrounding a parser with other other parsers.
between ::
    -- | open
    P a ->
    -- | close
    P b ->
    -- | body
    P c ->
    P c
between p q x = p *> x <* q

-- | Parser that succeeds at end of input string.
eof :: P ()
eof = void (tok "")

-- | Parse using a 'Read' instance.
pread :: (Read a) => P a
pread = P reads

-- | Wrapper for 'readParen'
preadParen :: Bool -> P a -> P a
preadParen req (P p) = P (readParen req p)

-- | Left-biased choice. Uses righthand-side if lefthand-side fails.
(<++) :: P a -> P a -> P a
P f <++ P g = P \s -> f s `orElse` g s
  where
    orElse [] ys = ys
    orElse xs _ = xs

infixr 3 <++
