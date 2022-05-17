module Main where

import Prelude

import Control.Monad.State (runState)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.List (List(..), findIndex, (!!), (:))
import Data.Maybe (Maybe(..))
import Data.Set (fromFoldable)
import Data.Tuple (Tuple(..))
import EFSM (EFSMConfig, Input, Output, processInput, updateVariables)
import EFSMRDFMap (TaskState(..), armForRdf, rdfForArm, rdfForEvents, rdfForTask, rdfForTasks, taskForRdf)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, modify, new, read, write)
import HTTPure (Headers, Method(..), Request, ResponseM, ServerM, badRequest, conflict, created, headers, internalServerError, noContent, notFound, ok', serve, toString, (!@))
import RDF (Graph, blankNode, defaultGraph, literalLang, namedNode, quad, serialize, triple)
import RDFPS.NTriplesParser (parse)
import RobotArm (D, a, b, e, s0)
import Text.Parsing.Parser (parseErrorMessage)

type ServerState = Record ( config :: EFSMConfig D, tasks :: List (Tuple String (Tuple Input TaskState)), events :: List (Tuple Int Output) )

baseURI :: String
baseURI = "http://localhost:8080/"

ntHeader :: Headers
ntHeader = headers [
  Tuple "Content-Type" "application/n-triples",
  Tuple "Accept" "application/n-triples"
]

router :: Ref ServerState -> Request -> ResponseM
router state { path: [], method: Get } = do
  { config } <- liftEffect $ read state
  ok' ntHeader $ serialize $ rdfForArm baseURI config
router state { path: [], method: Put, body: body } = do
  b <- toString body
  let gr = parse b
  case gr of
    Left parserError -> badRequest $ parseErrorMessage parserError
    Right gr' -> do
      let vars = armForRdf baseURI gr'
      case vars of 
        Nothing -> badRequest "Variables could not be extracted from RDF"
        Just vars' -> do
          { config, tasks, events } <- liftEffect $ read state
          let (Tuple success config') = runState (updateVariables e vars') config
          _ <- liftEffect $ write { config: config', tasks: tasks, events: events } state
          if success then noContent else conflict ""
router state { path: ["tasks"], method: Get } = do 
  { tasks } <- liftEffect $ read state
  ok' ntHeader (serialize $ rdfForTasks baseURI tasks)
router state { path: ["events"], method: Get } = ok' ntHeader (serialize $ rdfForEvents baseURI)
router state { path, method: Get }
  | path !@ 0 == "tasks" && not (path !@ 1 == "") = do
    { tasks } <- liftEffect $ read state
    case findIndex (\(Tuple idx _) -> path !@ 1 == idx) tasks of 
      Just taskListIdx -> case tasks !! taskListIdx of
        Just task -> ok' ntHeader (serialize $ rdfForTask baseURI taskListIdx task)
        Nothing -> internalServerError $ "TaskList index " <> show taskListIdx <> " not present but should be!"
      Nothing -> notFound
router state { path, method: Put, body }
  | path !@ 0 == "tasks" && not (path !@ 1 == "") = do
    b <- toString body
    let gr = parse b
    case gr of
      Left parserError -> badRequest $ parseErrorMessage parserError
      Right gr' -> do
        let input = taskForRdf (baseURI <> "tasks/" <> path !@ 1) gr'
        case input of 
          Nothing -> badRequest "Task could not be extracted from RDF"
          Just input' -> do
            _ <- liftEffect $ modify (\s -> { config: s.config, tasks: (Tuple (path !@ 1) (Tuple input' TaskAccepted)) : s.tasks, events: s.events }) state
            { config } <- liftEffect $ read state
            delay $ Milliseconds 5000.0
            let (Tuple output config') = runState (processInput e input') config
            _ <- case output of
              Nothing -> liftEffect $ modify (\s -> { config: config', tasks: s.tasks, events: s.events }) state
              Just output' -> liftEffect $ modify (\s -> { config: config', tasks: s.tasks, events: (Tuple (findNextEventIdx s.events) output') : s.events }) state
            created
  where
    findNextEventIdx :: List (Tuple Int Output) -> Int
    findNextEventIdx events = case maximum $ map (\(Tuple i _) -> i) events of 
      Nothing -> 0
      Just i -> i + 1
router _ _ = notFound

g :: Graph
g = fromFoldable [
  triple (namedNode "http://localhost:8080/") (namedNode "http://example.org/p") (literalLang "Daniel" "de"),
  quad (namedNode "http://example.org/a") (namedNode "http://example.org/p") (blankNode "b02") defaultGraph
]

main :: ServerM
main = do
  config <- new { config: Tuple s0 { pos: 1, closed: true, item: false }, tasks: (Tuple "task1" (Tuple a TaskFailed)) : (Tuple "task2" (Tuple b TaskFailed)) : Nil, events: Nil }
  serve 8080 (router config) $ log "Started server at http://localhost:8080/."
