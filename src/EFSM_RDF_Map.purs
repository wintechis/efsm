module EFSMRDFMap where

import Prelude

import Control.Alternative (guard)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Set (filter, findMax, fromFoldable, size, union)
import Data.Tuple (Tuple(..))
import EFSM (EFSMConfig, Input, Variables)
import RDF (Graph, Quad, Term, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, subject, termToBoolean, termToInt, termType, value)
import RDF.Prefixes (ldp, ra, rdf, xsd)
import RobotArm (D, a, b, c, d)

data TaskState = TaskAccepted | TaskRunning | TaskSuccessful | TaskFailed

rdfForArm :: String -> EFSMConfig D -> Graph
rdfForArm base (Tuple _ { pos, closed, item }) = fromFoldable [
  quad (namedNode $ base <> "#robotArm") (namedNode' rdf "type") (namedNode' ra "RobotArm") defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "tasks") (namedNode $ base <> "tasks/") defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "events") (namedNode $ base <> "events/") defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "pos") (literalType (show pos) (namedNode' xsd "integer")) defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "closed") (literalType (show closed) (namedNode' xsd "boolean")) defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "item") (literalType (show item) (namedNode' xsd "boolean")) defaultGraph
]

rdfForTasks :: String -> List (Tuple String (Tuple Input TaskState)) -> Graph
rdfForTasks base inputs = union (fromFoldable [
  quad (namedNode $ base <> "tasks/") (namedNode' rdf "type") (namedNode' ra "TaskContainer") defaultGraph
]) (fromFoldable $ inputsToContainsQuads base inputs)
  where
    inputsToContainsQuads :: String -> List (Tuple String (Tuple Input TaskState)) -> List Quad
    inputsToContainsQuads base' ((Tuple idx _) : is) =
      (quad (namedNode $ base <> "tasks/") (namedNode' ldp "contains") (namedNode $ base <> "tasks/" <> idx) defaultGraph)
      : inputsToContainsQuads base' is
    inputsToContainsQuads _ Nil = Nil

rdfForTask :: String -> Tuple String (Tuple Input TaskState) -> Graph
rdfForTask base (Tuple idx (Tuple input taskState)) = fromFoldable [
  quad (namedNode $ base <> "tasks/" <> idx) (namedNode' rdf "type") (task input) defaultGraph,
  quad (namedNode $ base <> "tasks/" <> idx) (namedNode' ra "taskState") (state taskState) defaultGraph
]
  where
    task :: Input -> Term
    task input' = if input' == a then namedNode' ra "MoveFrom1To2Task" else 
      if input' == b then namedNode' ra "MoveFrom2To3Task" else
      if input' == c then namedNode' ra "MoveFrom2To1Task" else
      if input' == d then namedNode' ra "MoveFrom3To2Task" else namedNode' ra "Task"
    state :: TaskState -> Term
    state TaskAccepted = namedNode' ra "accepted"
    state TaskRunning = namedNode' ra "running"
    state TaskSuccessful = namedNode' ra "successful"
    state TaskFailed = namedNode' ra "failed"

rdfForEvents :: String -> Graph
rdfForEvents base = fromFoldable [
  quad (namedNode $ base <> "events/") (namedNode' rdf "type") (namedNode' ra "EventContainer") defaultGraph
]

armForRdf :: String -> Graph -> Maybe (Variables D)
armForRdf base g = if correctSize && correctType then do
    guard $ size closedQuads == 1
    c <- object <$> findMax closedQuads
    cb <- termToBoolean c
    guard $ size itemQuads == 1
    i <- object <$> findMax itemQuads
    ib <- termToBoolean i
    guard $ size posQuads == 1
    p <- object <$> findMax posQuads
    pi <- termToInt p
    pure { pos: pi, closed: cb, item: ib }
  else Nothing
  where
    correctSize = size g == 6
    correctType = size (filter (\q -> subject q == (namedNode $ base <> "#robotArm") && predicate q == (namedNode' rdf "type")) g) == 1
    closedQuads = filter (\q -> subject q == (namedNode $ base <> "#robotArm") && predicate q == (namedNode' ra "closed")) g
    itemQuads = filter (\q -> subject q == (namedNode $ base <> "#robotArm") && predicate q == (namedNode' ra "item")) g
    posQuads = filter (\q -> subject q == (namedNode $ base <> "#robotArm") && predicate q == (namedNode' ra "pos")) g

taskForRdf :: String -> Graph -> Maybe Input
taskForRdf uri g = if correctSize then do
    guard $ size typeQuads == 1
    t <- object <$> findMax typeQuads
    guard $ termType t == "NamedNode"
    case value t of 
      "https://solid.ti.rw.fau.de/public/ns/robotArm#MoveFrom1To2Task" -> Just a
      "https://solid.ti.rw.fau.de/public/ns/robotArm#MoveFrom2To3Task" -> Just b
      "https://solid.ti.rw.fau.de/public/ns/robotArm#MoveFrom2To1Task" -> Just c
      "https://solid.ti.rw.fau.de/public/ns/robotArm#MoveFrom3To2Task" -> Just d
      _ -> Nothing
  else Nothing
  where
    correctSize = size g == 1
    typeQuads = filter (\q -> subject q == (namedNode uri) && predicate q == (namedNode' rdf "type")) g