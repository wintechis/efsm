module EFSMRDFMap where

import Prelude

import Control.Alternative (guard)
import Data.Maybe (Maybe(..))
import Data.Set (filter, findMax, fromFoldable, size)
import Data.Tuple (Tuple(..))
import EFSM (EFSMConfig, Variables)
import RDF (Graph, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, subject, termToBoolean, termToInt)
import RDF.Prefixes (ra, rdf, xsd)
import RobotArm (D)

rdfForArm :: String -> EFSMConfig D -> Graph
rdfForArm base (Tuple _ { pos, closed, item }) = fromFoldable [
  quad (namedNode $ base <> "#robotArm") (namedNode' rdf "type") (namedNode' ra "RobotArm") defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "tasks") (namedNode $ base <> "tasks/") defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "events") (namedNode $ base <> "events/") defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "pos") (literalType (show pos) (namedNode' xsd "integer")) defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "closed") (literalType (show closed) (namedNode' xsd "boolean")) defaultGraph,
  quad (namedNode $ base <> "#robotArm") (namedNode' ra "item") (literalType (show item) (namedNode' xsd "boolean")) defaultGraph
]

rdfForTasks :: String -> Graph
rdfForTasks base = fromFoldable [
  quad (namedNode $ base <> "tasks/") (namedNode' rdf "type") (namedNode' ra "TaskContainer") defaultGraph
]

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