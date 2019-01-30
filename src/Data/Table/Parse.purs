module Data.Table.Parse (module Data.Table.Parse) where

import Data.Table.Parse.Internal
import Prelude

import Data.Either (Either(..), either)
import Data.Either as Either
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
import Data.Traversable (class Traversable)
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup as Validation
import Text.Parsing.Parser (ParserT, ParseError, runParser)
import Text.Parsing.Parser.String as PS

parse ::
  forall colId rowId column row cell.
  Ord rowId => Ord colId => Ord cell => Ord colId =>
  (String -> Maybe cell) ->
  (String -> Maybe colId) ->
  (NonEmptyList cell -> Maybe row) ->
  (String -> Maybe rowId) ->
  (NonEmptyList cell -> Maybe column) ->
  String ->
  Either (Error rowId colId cell) (Table rowId colId cell row column)
parse mkCell mkColId mkCol mkRowId mkRow s =
  either (Left <<< MkParseError) (toTyped mkCell mkColId mkCol mkRowId mkRow) $
  runParser s table

table :: forall m. Monad m => ParserT String m (List (List String))
table = do
  PS.skipSpaces
  h <- header
  void $ PS.char '\n' *> line
  rs <- rows
  pure $ h : rs

toTyped ::
  forall colId rowId column row cell.
  Ord rowId => Ord colId => Ord cell => Ord colId =>
  (String -> Maybe cell) ->
  (String -> Maybe colId) ->
  (NonEmptyList cell -> Maybe row) ->
  (String -> Maybe rowId) ->
  (NonEmptyList cell -> Maybe column) ->
  List (List String) ->
  Either (Error rowId colId cell) (Table rowId colId cell row column)
toTyped mkCell mkColId mkCol mkRowId mkRow rawRows =
  if sameLengths rawRows
  then maybe (Left MkEmpty) refine $ nonEmptify rawRows
  else Left MkRaggedRows
  where
    mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
    mapLeft f = either (Left <<< f) Right
    collectErrors :: forall a b t. Semigroup a => Traversable t => t (Validation.V a b) -> Either a (t b)
    collectErrors = Validation.toEither <<< Traversable.sequence
    tryMk :: forall a b c. (a -> Maybe b) -> (a -> c) -> a -> Validation.V (List c) b
    tryMk mk err a = Validation.V <<< Either.note (List.singleton $ err a) <<< mk $ a
    refine rows = do
      cells <- either (Left <<< MkRowErrors) (Right <<< join) <<< collectErrors $ rowToCells <$> NonEmpty.tail rows
      mapLeft MkTableErrors <<< Table.mk mkCol mkRow <<< Map.fromFoldable $ cells
      where
        rowToCells (NonEmptyList (NonEmpty rowId cells)) = Validation.V <<< mapLeft List.singleton $ do
          colIds <- mapLeft MkBadColIds <<< collectErrors <<< map (tryMk mkColId identity) <<< NonEmpty.tail <<< NonEmpty.head $ rows
          mapLeft MkZipperErrors <<< collectErrors $ List.zipWith zipper colIds cells
          where
            zipper colId cell = ado
              rowId' <- tryMk mkRowId MkBadRowId rowId
              cell' <- tryMk mkCell MkBadCell cell
              in Tuple (Tuple rowId' colId) cell'

data ZipperError = MkBadRowId String | MkBadCell String
derive instance genericZError :: Generic ZipperError _
instance showZError :: Show ZipperError where
  show = genericShow

data RowToCellsError = MkBadColIds (List String) | MkZipperErrors (List ZipperError)
derive instance genericRtcError :: Generic RowToCellsError _
instance showRtcError :: Show RowToCellsError where
 show = genericShow

data Error rowId colId cell
  = MkTableErrors (Set (Table.Error rowId colId cell))
  | MkRowErrors (List RowToCellsError)
  | MkRaggedRows
  | MkEmpty
  | MkParseError ParseError
derive instance genericError :: Generic (Error rowId colId cell) _

instance showError :: (Show rowId, Show colId, Show cell) => Show (Error rowId colId cell) where
  show = genericShow
