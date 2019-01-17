module Data.Table.Parse.Internal where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Set as Set
import Data.String (joinWith, trim) as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators as Com
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as Tok

spaces :: forall m. Functor m => Monad m => ParserT String m Unit
spaces = void $ Array.many (PS.char ' ')

colSep :: forall m. Monad m => ParserT String m Unit
colSep = void $ spaces *> PS.string "|" <* spaces

word :: forall m. Functor m => Monad m => ParserT String m String
word = String.fromCharArray <$> Array.some Tok.alphaNum

colContent :: forall m. Monad m => ParserT String m String
colContent =
  String.trim <<< String.joinWith "" <<< Array.fromFoldable <$>
  Com.many1Till ((<>) <$> word <*> PS.whiteSpace) (Com.lookAhead colSep)

row :: forall m. Monad m => ParserT String m (List String)
row = colSep *> Com.sepEndBy1 colContent colSep

rows :: forall m. Monad m => ParserT String m (List (List String))
rows = Com.sepEndBy1 row (PS.char '\n')

header :: forall m. Monad m => ParserT String m (List String)
header = (mempty : _) <$> (colSep *> row)

line :: forall m. Monad m => ParserT String m (List Char)
line = Com.manyTill PS.anyChar (PS.char '\n')

sameLengths ::
  forall f t a.
  Foldable f => Functor f => Foldable t => Functor t =>
  f (t a) -> Boolean
sameLengths = (==) 1 <<< Set.size <<< Set.fromFoldable <<< map length
  where
    length :: forall g b. Foldable g => Functor g => g b -> Int
    length = Foldable.length

nonEmptify :: forall a. List (List a) -> Maybe (NonEmptyList (NonEmptyList a))
nonEmptify = NonEmpty.fromList <=< traverse NonEmpty.fromList
