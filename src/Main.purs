module Main where

import Prelude

import Control.Monad.State (runState)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (fromFoldable)
import Data.Tuple (Tuple(..))
import EFSM (EFSMConfig, updateVariables)
import EFSMRDFMap (armForRdf, rdfForArm, rdfForEvents, rdfForTasks)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read, write)
import HTTPure (Headers, Method(..), Request, ResponseM, ServerM, badRequest, conflict, headers, noContent, notFound, ok', serve, toString)
import RDF (Graph, blankNode, defaultGraph, literalLang, namedNode, quad, serialize, triple)
import RDFPS.NTriplesParser (parse)
import RobotArm (D, e, s0)
import Text.Parsing.Parser (parseErrorMessage)

baseURI :: String
baseURI = "http://localhost:8080/"

ntHeader :: Headers
ntHeader = headers [
  Tuple "Content-Type" "application/n-triples",
  Tuple "Accept" "application/n-triples"
]

initialEFSMConfig :: EFSMConfig D
initialEFSMConfig = Tuple s0 { pos: 1, closed: true, item: false }

router :: Ref (EFSMConfig D) -> Request -> ResponseM
router config { path: [], method: Get } = do
  c <- liftEffect $ read config
  ok' ntHeader $ serialize $ rdfForArm baseURI c
router config { path: [], method: Put, body: body } = do
  b <- toString body
  let gr = parse b
  case gr of
    Left parserError -> badRequest $ parseErrorMessage parserError
    Right gr' -> do
      let vars = armForRdf baseURI gr'
      case vars of 
        Nothing -> badRequest "Variables could not be extracted from RDF"
        Just vars' -> do
          c <- liftEffect $ read config
          let (Tuple success c') = runState (updateVariables e vars') c
          _ <- liftEffect $ write c' config
          if success then noContent else conflict ""
router config { path: ["tasks"], method: Get } = ok' ntHeader (serialize $ rdfForTasks baseURI)
router config { path: ["events"], method: Get } = ok' ntHeader (serialize $ rdfForEvents baseURI)
router _ _ = notFound

g :: Graph
g = fromFoldable [
  triple (namedNode "http://localhost:8080/") (namedNode "http://example.org/p") (literalLang "Daniel" "de"),
  quad (namedNode "http://example.org/a") (namedNode "http://example.org/p") (blankNode "b02") defaultGraph
]

main :: ServerM
main = do
  config <- new initialEFSMConfig
  serve 8080 (router config) $ log "Started server at http://localhost:8080/."
