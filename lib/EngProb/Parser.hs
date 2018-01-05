{-|
Module      : EngProb.Parser
Description : Parser functions
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module defines assorted parser functions.
-}

module EngProb.Parser
    ( countSepBy
    , listWithLeadingCount
    , parseAll
    , skipSpace1
    , wholeInput
    ) where

import           Control.Monad (void, when)
import           Data.Attoparsec.Text
                    ( Parser
                    , count
                    , decimal
                    , endOfInput
                    , parseOnly
                    , skip
                    , skipMany1
                    , skipSpace
                    )
import           Data.Char (isSpace)
import           Data.Text (Text)

-- |Skip at least one space
skipSpace1 :: Parser () -- ^ Parser
skipSpace1 = skipMany1 (skip isSpace)

-- |Parse specified number of items separated by @sep@
countSepBy ::
    Int             -- ^ Number of items
    -> Parser a     -- ^ Item parser
    -> Parser sep   -- ^ Separator
    -> Parser [a]   -- ^ List parser
countSepBy 0 _ _ = return []
countSepBy 1 f _ = (:[]) <$> f
countSepBy n f sep = do
    item <- f
    items <- count (n - 1) $ do void sep; f
    return $ item : items

-- |Parse list of items with preceding count
listWithLeadingCount ::
    Parser a        -- ^ Item parser
    -> Parser sep   -- ^ Separator
    -> Parser [a]   -- ^ List parser
listWithLeadingCount f sep = do
    n <- decimal
    when (n > 0) (void sep)
    countSepBy n f sep

-- |Build parser which will ignore leading and trailing whitespace
wholeInput ::
    Parser a    -- ^ Base parser
    -> Parser a -- ^ Parser
wholeInput f = skipSpace *> f <* skipSpace <* endOfInput

-- |Parse input ignoring leading and trailing whitespace
parseAll ::
    Parser a            -- ^ Base parser
    -> Text             -- ^ Text
    -> Either String a  -- ^ Parse result
parseAll = parseOnly . wholeInput