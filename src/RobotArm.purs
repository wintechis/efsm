module RobotArm where

import Prelude

import Control.Monad.State (runState)
import Data.Foldable (find)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Set (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import EFSM (AdmissibilityFunction, EFSM(..), EnablingFunction, Input(..), Output(..), SymbolicState(..), Transitions, UpdateFunction, Variables, inputSet, processInput, updateVariables, (==>))
import Effect (Effect)
import Effect.Console (logShow)
import RDF (Graph, datatype, literalType, namedNode, object, predicate, subject, termType, triple, value)

-- S
s0 :: SymbolicState
s0 = SymbolicState "s0"
s1 :: SymbolicState
s1 = SymbolicState "s1"
s2 :: SymbolicState
s2 = SymbolicState "s2"
s3 :: SymbolicState 
s3 = SymbolicState "s3"
s4 :: SymbolicState
s4 = SymbolicState "s4"
s5 :: SymbolicState
s5 = SymbolicState "s5"

-- I
a :: Input
a = Input "A"
b :: Input
b = Input "B"
c :: Input
c = Input "C"
d :: Input
d = Input "D"

-- O
p :: Output
p = Output "P"

-- D
type D = ( pos :: Int, closed :: Boolean, item :: Boolean )

-- F
f1 :: EnablingFunction D
f1 _ = true

f2 :: EnablingFunction D
f2 { pos: _, closed, item: _ } = closed

f3 :: EnablingFunction D
f3 { pos: _, closed, item: _ } = not closed

-- U
u1 :: UpdateFunction D
u1 { pos: _, closed, item } = { pos: 1, closed: closed, item: item }

u2 :: UpdateFunction D
u2 { pos: _, closed, item } = { pos: 2, closed: closed, item: item }

u3 :: UpdateFunction D
u3 { pos: _, closed, item } = { pos: 3, closed: closed, item: item }

u4 :: UpdateFunction D
u4 { pos, closed, item: _ } = { pos: pos, closed: closed, item: true }

u5 :: UpdateFunction D
u5 { pos, closed, item: _ } = { pos: pos, closed: closed, item: false }

u6 :: UpdateFunction D
u6 { pos: _, closed, item: _ } = { pos: 2, closed: closed, item: false }

-- T
ts :: Transitions D
ts = [
    s0 /\ f1 /\ Just a /\ unit  ==> s1 /\ u2 /\ Nothing /\ unit,
    s1 /\ f1 /\ Just c /\ unit  ==> s0 /\ u1 /\ Nothing /\ unit,
    s1 /\ f1 /\ Just b /\ unit  ==> s2 /\ u3 /\ Nothing /\ unit,
    s2 /\ f1 /\ Just d /\ unit  ==> s1 /\ u2 /\ Nothing /\ unit,
    s2 /\ f3 /\ Nothing /\ unit ==> s5 /\ u4 /\ Nothing /\ unit,
    s3 /\ f3 /\ Nothing /\ unit ==> s0 /\ u5 /\ Just p /\ unit,
    s3 /\ f1 /\ Just a /\ unit  ==> s4 /\ u2 /\ Nothing /\ unit,
    s4 /\ f1 /\ Just c /\ unit  ==> s3 /\ u1 /\ Nothing /\ unit,
    s4 /\ f1 /\ Just b /\ unit  ==> s5 /\ u3 /\ Nothing /\ unit,
    s5 /\ f2 /\ Just d /\ unit  ==> s4 /\ u2 /\ Nothing /\ unit,
    s5 /\ f3 /\ Just d /\ unit  ==> s1 /\ u6 /\ Nothing /\ unit
  ]

-- A
a1 :: AdmissibilityFunction D
a1 (SymbolicState "s0") { pos: 1, closed: _, item: false } = true
a1 (SymbolicState "s1") { pos: 2, closed: _, item: false } = true
a1 (SymbolicState "s2") { pos: 3, closed: _, item: false } = true
a1 (SymbolicState "s3") { pos: 1, closed: _, item: true } = true
a1 (SymbolicState "s4") { pos: 2, closed: _, item: true } = true
a1 (SymbolicState "s5") { pos: 3, closed: _, item: true } = true
a1 _ _ = false

-- E
e :: EFSM D
e = EFSM ts a1

variablesToRdf :: Variables D -> Graph
variablesToRdf { pos, closed, item } = fromFoldable [
  triple (namedNode "http://localhost:8080/") (namedNode "http://www.w3.org/2001/XMLSchema#") (namedNode "http://example.org/RobotArm"),
  triple (namedNode "http://localhost:8080/") (namedNode "http://example.org/pos") (literalType (show pos) (namedNode "http://www.w3.org/2001/XMLSchema#integer")),
  triple (namedNode "http://localhost:8080/") (namedNode "http://example.org/closed") (literalType (show closed) (namedNode "http://www.w3.org/2001/XMLSchema#boolean")),
  triple (namedNode "http://localhost:8080/") (namedNode "http://example.org/item") (literalType (show item) (namedNode "http://www.w3.org/2001/XMLSchema#boolean"))
]

rdfToVariables :: Graph -> Maybe (Variables D)
rdfToVariables g = do
  posString <- getPos g
  pos <- fromString posString
  closedString <- getClosed g
  closed <- Just (closedString == "true")
  itemString <- getItem g
  item <- Just (itemString == "true")
  pure { pos: pos, closed: closed, item: item }
  where
    getPos :: Graph -> Maybe String
    getPos g' = value <$> object <$> find (\q -> (subject q) == namedNode "http://localhost:8080/" && (predicate q) == namedNode "http://example.org/pos" && (termType $ object q) == "Literal" && (datatype $ object q) == Just (namedNode "http://www.w3.org/2001/XMLSchema#integer")) g'
    getClosed g' = value <$> object <$> find (\q -> (subject q) == namedNode "http://localhost:8080/" && (predicate q) == namedNode "http://example.org/closed" && (termType $ object q) == "Literal" && (datatype $ object q) == Just (namedNode "http://www.w3.org/2001/XMLSchema#boolean")) g'
    getItem g' = value <$> object <$> find (\q -> (subject q) == namedNode "http://localhost:8080/" && (predicate q) == namedNode "http://example.org/item" && (termType $ object q) == "Literal" && (datatype $ object q) == Just (namedNode "http://www.w3.org/2001/XMLSchema#boolean")) g'

main :: Effect Unit
main = do
  logShow $ inputSet e
  logShow $ runState (updateVariables e { pos: 1, closed: true, item: false }) (Tuple s0 { pos: 1, closed: false, item: false })
  logShow $ runState (updateVariables e { pos: 2, closed: true, item: false }) (Tuple s0 { pos: 1, closed: false, item: false })
  logShow $ runState (processInput e a) (Tuple s0 { pos: 1, closed: false, item: false})
  logShow $ runState (processInput e b) (Tuple s0 { pos: 1, closed: false, item: false})