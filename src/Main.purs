module Main where

import Prelude

import Control.Monad.State (runState)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.List (List(..), findIndex, snoc, updateAt, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import EFSM (EFSMConfig, Input, Output, epsilon, processInput, updateVariables)
import EFSMRDFMap (TaskState(..), armForRdf, rdfForArm, rdfForEvent, rdfForEvents, rdfForTask, rdfForTasks, taskForRdf)
import Effect.Aff (Aff, Milliseconds(..), delay, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Effect.Random (random)
import Effect.Ref (Ref, modify, new, read, write)
import Effect.Timer (setTimeout)
import HTTPure (Headers, Method(..), Request, Response, ResponseM, ServerM, badRequest, conflict, created, headers, internalServerError, noContent, notFound, ok', serve, toString, (!@))
import RDF (serialize)
import RDFPS.NTriplesParser (parse)
import RobotArm (D, e, s0)
import Text.Parsing.Parser (parseErrorMessage)

type ServerState = Record ( config :: EFSMConfig D, tasks :: List (Tuple Int (Tuple Input TaskState)), events :: List (Tuple Int (Tuple DateTime Output)) )

baseURI :: String
baseURI = "http://localhost:8080/"

ntHeader :: Headers
ntHeader = headers [
  Tuple "Content-Type" "application/n-triples",
  Tuple "Accept" "application/n-triples"
]

router :: Ref ServerState -> Request -> ResponseM
-- JSON
router state { path: ["json"], method: Get } = do
  { config: Tuple _ vars } <- liftEffect $ read state
  ok' (headers [ Tuple "Content-Type" "application/json", Tuple "Access-Control-Allow-Origin" "*"]) ("{\"pos\":" <> show vars.pos <> ",\"closed\":" <> show vars.closed <> ",\"item\":" <> show vars.item <> "}")
-- Get arm state
router state { path: [], method: Get } = do
  { config } <- liftEffect $ read state
  ok' ntHeader $ serialize $ rdfForArm baseURI config
-- Put variables
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
-- Get task container
router state { path: ["tasks"], method: Get } = do 
  { tasks } <- liftEffect $ read state
  ok' ntHeader (serialize $ rdfForTasks baseURI tasks)
-- Get event container
router state { path: ["events"], method: Get } = do
  { events } <- liftEffect $ read state
  ok' ntHeader (serialize $ rdfForEvents baseURI events)
-- Get a specific task
router state { path, method: Get }
  | path !@ 0 == "tasks" && not (path !@ 1 == "") = do
    { tasks } <- liftEffect $ read state
    case findIndex (\(Tuple idx _) -> path !@ 1 == show idx) tasks of 
      Just taskListIdx -> case tasks !! taskListIdx of
        Just task -> ok' ntHeader (serialize $ rdfForTask baseURI taskListIdx task)
        Nothing -> internalServerError $ "TaskList index " <> show taskListIdx <> " not present but should be!"
      Nothing -> notFound
-- Get a specific event
router state { path, method: Get }
  | path !@ 0 == "events" && not (path !@ 1 == "") = do
    { events } <- liftEffect $ read state
    case findIndex (\(Tuple idx _) -> path !@ 1 == show idx) events of 
      Just eventListIdx -> case events !! eventListIdx of
        Just event -> ok' ntHeader (serialize $ rdfForEvent baseURI eventListIdx event)
        Nothing -> internalServerError $ "EventList index " <> show eventListIdx <> " not present but should be!"
      Nothing -> notFound
-- Post a task
router state { path: ["tasks"], method: Post, body } = do
    b <- toString body
    let gr = parse b
    case gr of
      Left parserError -> badRequest $ parseErrorMessage parserError
      Right gr' -> do
        { tasks } <- liftEffect $ read state
        let taskIdx = findNextTaskIdx tasks
        let input = taskForRdf gr'
        case input of 
          Nothing -> badRequest "Task could not be extracted from RDF"
          Just input' -> do
            _ <- liftEffect $ modify (\s -> { config: s.config, tasks: snoc s.tasks (Tuple taskIdx (Tuple input' TaskAccepted)), events: s.events }) state
            makeResponse
  where
    makeResponse :: Aff Response
    makeResponse = created
router _ _ = notFound

runTasksAndEpsilons :: Ref ServerState -> Aff Unit
runTasksAndEpsilons state = do
  r <- liftEffect $ random
  res <- if r < 0.5 then
    runEpsilons state
  else
    runTasks state
  _ <- liftEffect $ setTimeout 500 $ runAff_ (\_ -> pure unit) $ runTasksAndEpsilons state
  pure res

runEpsilons :: Ref ServerState -> Aff Unit
runEpsilons state = do
  { config, tasks, events } <- liftEffect $ read state
  let (Tuple (Tuple success output) config') = runState (epsilon e) config
  if success then delay $ Milliseconds 5000.0 else pure unit
  now <- liftEffect $ nowDateTime
  let events' = case output of
        Nothing -> events
        Just o -> snoc events (Tuple (findNextEventIdx events) (Tuple now o))
  liftEffect $ write { config: config', tasks: tasks, events: events' } state
  pure unit

runTasks :: Ref ServerState -> Aff Unit
runTasks state = do
  { config, tasks, events } <- liftEffect $ read state
  let idx = findIndex (\(Tuple _ (Tuple _ taskState)) -> taskState == TaskAccepted) tasks
  case idx of
    Nothing -> pure unit
    Just idx' -> do
      let input = tasks !! idx'
      case input of
        Nothing -> pure unit
        Just (Tuple taskId (Tuple input' _)) -> do
          let tasks' = case updateAt idx' (Tuple taskId (Tuple input' TaskRunning)) tasks of 
                Nothing -> tasks
                Just t -> t
          liftEffect $ write { config: config, tasks: tasks', events: events } state
          delay $ Milliseconds 5000.0
          now <- liftEffect $ nowDateTime
          let (Tuple (Tuple success output) config') = runState (processInput e input') config
          let events' = case output of
                Nothing -> events
                Just o -> snoc events (Tuple (findNextEventIdx events) (Tuple now o))
          let tasks'' = case updateAt idx' (Tuple taskId (Tuple input' (if success then TaskSuccessful else TaskFailed))) tasks of 
                Nothing -> tasks
                Just t -> t
          liftEffect $ write { config: config', tasks: tasks'', events: events' } state
          pure unit
  pure unit

findNextEventIdx :: List (Tuple Int (Tuple DateTime Output)) -> Int
findNextEventIdx events = case maximum $ map (\(Tuple i _) -> i) events of 
  Nothing -> 0
  Just i -> i + 1

findNextTaskIdx :: List (Tuple Int (Tuple Input TaskState)) -> Int
findNextTaskIdx events = case maximum $ map (\(Tuple i _) -> i) events of 
  Nothing -> 0
  Just i -> i + 1

main :: ServerM
main = do
  state <- new { config: Tuple s0 { pos: 1, closed: true, item: false }, tasks: Nil, events: Nil }
  _ <- liftEffect $ setTimeout 500 $ runAff_ (\_ -> pure unit) $ runTasksAndEpsilons state
  serve 8080 (router state) $ log "Started server at http://localhost:8080/."
