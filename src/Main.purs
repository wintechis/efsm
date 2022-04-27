module Main where

import Prelude

import Data.Set (fromFoldable)
import Effect.Console (log)
import HTTPure (Request, ResponseM, ServerM, header, ok, ok', serve)
import RDF (Graph, blankNode, defaultGraph, literalLang, namedNode, quad, serialize, triple)

router :: Request -> ResponseM
router { path: [] } = ok "Bla"
router { path: ["hi", "ho"] } = ok' (header "Content-Type" "text/turtle") "<> a <http://example.org/Bla> ."
router _ = ok "default"

g :: Graph
g = fromFoldable [
  triple (namedNode "http://localhost:8080/") (namedNode "http://example.org/p") (literalLang "Daniel" "de"),
  quad (namedNode "http://example.org/a") (namedNode "http://example.org/p") (blankNode "b02") defaultGraph
]

main :: ServerM
main = serve 8080 router $ log $ serialize g--log "Started server at port 8080."
