module RDF where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Effect (Effect)
import Effect.Class.Console (logShow)

data Term = NamedNode String | BlankNode String | LiteralLang String String | LiteralType String Term | Variable String | DefaultGraph
derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term
instance showTerm :: Show Term where
  show (NamedNode s) = "<" <> s <> ">"
  show (BlankNode s) = "_:" <> s
  show (LiteralLang s l) = "\"" <> s <> "\"@" <> l
  show (LiteralType s t) = "\"" <> s <> "\"^^" <> show t
  show (Variable s) = "?" <> s
  show DefaultGraph = ""

data Quad = Quad Term Term Term Term
derive instance eqQuad :: Eq Quad
derive instance ordQuad :: Ord Quad
instance showQuad :: Show Quad where
  show (Quad s p o DefaultGraph) = show s <> " " <> show p <> " " <> show o <> " ."
  show (Quad s p o g) = show s <> " " <> show p <> " " <> show o <> " " <> show g <> " ."

type Graph = Set Quad

namedNode :: String -> Term
namedNode s = NamedNode s

blankNode :: String -> Term
blankNode s = BlankNode s

literalType :: String -> Term -> Maybe Term
literalType s (NamedNode t) = Just (LiteralType s (NamedNode t))
literalType _ _ = Nothing

literalLang :: String -> String -> Maybe Term
literalLang s l = Just (LiteralLang s l)

variable :: String -> Term
variable s = Variable s

defaultGraph :: Term
defaultGraph = DefaultGraph

quad :: Term -> Term -> Term -> Term -> Quad
quad s p o g = Quad s p o g

triple :: Term -> Term -> Term -> Quad
triple s p o = Quad s p o defaultGraph

termType :: Term -> String
termType (NamedNode _) = "NamedNode"
termType (BlankNode _) = "BlankNode"
termType (LiteralLang _ _) = "Literal"
termType (LiteralType _ _) = "Literal"
termType (Variable _) = "Variable"
termType DefaultGraph = "DefaultGraph"

value :: Term -> String
value (NamedNode v) = v
value (BlankNode v) = v
value (LiteralLang v _) = v
value (LiteralType v _) = v
value (Variable v) = v
value DefaultGraph = ""

language :: Term -> Maybe String
language (LiteralLang _ l) = Just l
language _ = Nothing

datatype :: Term -> Maybe Term
datatype (LiteralType _ (NamedNode t)) = Just (NamedNode t)
datatype _ = Nothing

q :: Quad
q = triple (namedNode "http://example.org/a") (namedNode "http://example.org/p") (fromMaybe DefaultGraph (literalType "3" (namedNode "http://www.w3.org/2001/XMLSchema#integer")))

main :: Effect Unit
main = do
  logShow q