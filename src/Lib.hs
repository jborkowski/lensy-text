{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
module Lib where

import           Control.Lens
import           Data.Char         (isSpace)
import           Data.Function     (on)
import qualified Data.Text         as T
import           Text.RawString.QQ (r)


-- takingN :: Int -> Traversal' T.Text T.Text
-- takingN n handler text =
--   liftA2 (<>) (handler prefix) (pure suffix)
--     where
--       (prefix, suffix) = T.splitAt n text

-- droppingN :: Int -> Traversal' T.Text T.Text
-- droppingN n handler text =
--   liftA2 (<>) (pure prefix) (handler suffix)
--     where
--      (prefix, suffix) = T.splitAt n text

takingN :: Int -> Traversal' T.Text T.Text
takingN n = splittingAt n . _1

droppingN :: Int -> Traversal' T.Text T.Text
droppingN n = splittingAt n . _2

splittingAt :: Int -> Iso' T.Text (T.Text, T.Text)
splittingAt n = iso to' from'
  where
    to' :: T.Text -> (T.Text, T.Text)
    to' = T.splitAt n
    from' :: (T.Text, T.Text) -> T.Text
    from' (a, b) = a <> b

foo :: T.Text -> String
foo = undefined

-- space preserving words
words' :: Traversal' T.Text T.Text
words' = splitOnPredicate isSpace

splitOnPredicate :: (Char -> Bool) -> IndexedTraversal' Int T.Text T.Text
splitOnPredicate p = indexing (collecting . traversed . _Right)
  where
    collecting :: Iso' T.Text [Either T.Text T.Text]
    collecting = iso collect collapse
    collect :: T.Text -> [Either T.Text T.Text]
    collect t = map toEither $ T.groupBy ((==) `on` p) t
    collapse :: [Either T.Text T.Text] -> T.Text
    collapse = foldOf (traversed . both)
    toEither t
      | T.all p t = Left t
      | otherwise   = Right t

aliceText :: T.Text
aliceText = "Alice was beginning to get very tired of sitting by her sister on the bank, and of having nothing to do: once or twice she had peeped into the book her sister was reading, but it had no pictures or conversations in it, without pictures or conversations?"

rows :: IndexedTraversal' Int T.Text T.Text
rows = splitOnPredicate (== '\n')

columns :: IndexedTraversal' Int T.Text T.Text
columns = splitOnPredicate (== '|')

table :: T.Text
table = [r|name    | city
Darek   | Portland
Elton   | Proto|]
