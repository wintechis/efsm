module RDF where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)

class Term a where
  termType :: a -> String
  value :: a -> String

data NamedNode = NamedNode String
instance termNamedNode :: Term NamedNode where
  termType (NamedNode _) = "NamedNode"
  value (NamedNode v) = v
derive instance eqNamedNode :: Eq NamedNode
derive instance ordNamedNode :: Ord NamedNode
instance showNamedNode :: Show NamedNode where
  show (NamedNode s) = "<" <> show s <> ">"

data BlankNode = BlankNode String
instance termBlankNode :: Term BlankNode where
  termType (BlankNode _) = "BlankNode"
  value (BlankNode v) = v
derive instance eqBlankNode :: Eq BlankNode
derive instance ordBlankNode :: Ord BlankNode
instance showBlankNode :: Show BlankNode where
  show (BlankNode s) = "_:" <> show s

data Literal = LiteralLang String String | LiteralType String NamedNode
instance termLiteral :: Term Literal where
  termType (LiteralLang _ _) = "Literal"
  termType (LiteralType _ _) = "Literal"
  value (LiteralLang v _) = v
  value (LiteralType v _) = v
derive instance eqLiteral :: Eq Literal
derive instance ordLiteral :: Ord Literal
instance showLiteral :: Show Literal where
  show (LiteralLang s l) = "\"" <> show s <> "\"@" <> show l
  show (LiteralType s t) = "\"" <> show s <> "\"^^" <> show t

data Variable = Variable String
instance termVariable :: Term Variable where
  termType (Variable _) = "Variable"
  value (Variable v) = v
derive instance eqVariable :: Eq Variable
derive instance ordVariable :: Ord Variable
instance showVariable :: Show Variable where
  show (Variable s) = "?" <> show s

data DefaultGraph =  DefaultGraph
instance termDefaultGraph :: Term DefaultGraph where
  termType DefaultGraph = "DefaultGraph"
  value DefaultGraph = ""
derive instance eqDefaultGraph :: Eq DefaultGraph
derive instance ordDefaultGraph :: Ord DefaultGraph
instance showDefaultGraph :: Show DefaultGraph where
  show DefaultGraph = ""

data Quad a b c d = Quad a b c d
derive instance eqQuad :: Eq (Quad a b c d)
derive instance ordQuad :: Term a => Ord (Quad a b c d)
instance showQuad :: Show (Quad a b c d) where
--  show (Quad s p o DefaultGraph) = show s <> " " <> show p <> " " <> show o <> " ."
--  show (Quad s p o g) = show s <> " " <> show p <> " " <> show o <> " " <> show g <> " ."
  show q = "sd"

type Graph a b c d  = Set (Quad a b c d)

namedNode :: String -> NamedNode
namedNode s = NamedNode s

blankNode :: String -> BlankNode
blankNode s = BlankNode s

literalType :: String -> NamedNode -> Literal
literalType s t = LiteralType s t

literalLang :: String -> String -> Literal
literalLang s l = LiteralLang s l

variable :: String -> Variable
variable s = Variable s

defaultGraph :: DefaultGraph
defaultGraph = DefaultGraph

quad :: forall a b c d. (Term a) => (Term b) => (Term c) => (Term d) => a -> b -> c -> d -> Quad a b c d
quad s p o g = Quad s p o g

triple ::forall a b c. (Term a) => (Term b) => (Term c) => a -> b -> c -> Quad a b c DefaultGraph
triple s p o = Quad s p o defaultGraph

language :: Literal -> Maybe String
language (LiteralLang _ l) = Just l
language _ = Nothing

datatype :: Literal -> Maybe NamedNode
datatype (LiteralType _ (NamedNode t)) = Just (NamedNode t)
datatype _ = Nothing

q :: Quad NamedNode NamedNode Literal DefaultGraph
q = triple (namedNode "http://example.org/a") (namedNode "http://example.org/p") (literalType "3" (namedNode "http://www.w3.org/2001/XMLSchema#integer"))