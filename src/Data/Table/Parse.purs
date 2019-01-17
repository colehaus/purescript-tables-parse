module Data.Table.Parse (parseTable, Error) where

import Prelude

import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import Data.Table (Table)
import Data.Table as Table
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParserT, ParseError, runParser)
import Text.Parsing.Parser.String as PS

import Data.Table.Parse.Internal

parseTable ::
  forall colId rowId column row cell.
  Ord rowId => Ord colId => Ord cell => Ord colId =>
  (String -> cell) ->
  (String -> colId) ->
  (NonEmptyList cell -> Maybe row) ->
  (String -> rowId) ->
  (NonEmptyList cell -> Maybe column) ->
  String ->
  Either (Error rowId colId cell) (Table rowId colId cell row column)
parseTable mkCell mkColId mkCol mkRowId mkRow s =
  either (Left <<< MkParseError) (toTable mkCell mkColId mkCol mkRowId mkRow) $
  runParser s table

table :: forall m. Monad m => ParserT String m (List (List String))
table = do
  PS.skipSpaces
  h <- header
  void $ PS.char '\n' *> line
  rs <- rows
  pure $ h : rs

toTable ::
  forall colId rowId column row cell.
  Ord rowId => Ord colId => Ord cell => Ord colId =>
  (String -> cell) ->
  (String -> colId) ->
  (NonEmptyList cell -> Maybe row) ->
  (String -> rowId) ->
  (NonEmptyList cell -> Maybe column) ->
  List (List String) ->
  Either (Error rowId colId cell) (Table rowId colId cell row column)
toTable mkCell mkColId mkCol mkRowId mkRow rawRows =
  if sameLengths rawRows
  then maybe (Left MkEmpty) refine $ nonEmptify rawRows
  else Left MkRaggedRows
  where
    refine rows =
      either (Left <<< MkTableErrors) Right <<<
      Table.mk mkCol mkRow <<<
      Map.fromFoldable $
      rowToCells =<< NonEmpty.tail rows
      where
        rowToCells (NonEmptyList (NonEmpty rowId cells)) = List.zipWith zipper colIds cells
          where
            zipper colId cell = Tuple (Tuple (mkRowId rowId) colId) (mkCell cell)
            colIds = map mkColId <<< NonEmpty.tail <<< NonEmpty.head $ rows

data Error rowId colId cell
  = MkTableErrors (Set (Table.Error rowId colId cell))
  | MkRaggedRows
  | MkEmpty
  | MkParseError ParseError
derive instance genericError :: Generic (Error rowId colId cell) _

instance showError ::
  (Show rowId, Show colId, Show cell) =>
  Show (Error rowId colId cell) where
  show = genericShow
