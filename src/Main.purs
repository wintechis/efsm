module Main where

import Prelude

import EFSM (Assignment(..), Closed(..), Item(..), Pos(..))
import Effect.Console (log)
import HTTPure (Request, ResponseM, ServerM, header, ok, ok', serve)

getAssignment :: Assignment -> ResponseM
getAssignment (Assignment pos item closed) = ok' (header "Content-Type" "text/turtle") ("""
@prefix : <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<> :pos """ <> show pos <> """ ;
   :item """ <> show item <> """ ;
   :closed """ <> show closed <> """ .
""")

router :: Request -> ResponseM
router { path: [] } = getAssignment (Assignment (Pos 1) (Closed false) (Item false))
router { path: ["hi", "ho"] } = ok' (header "Content-Type" "text/turtle") "<> a <http://example.org/Bla> ."
router _ = ok "default"

main :: ServerM
main = serve 8080 router $ log "Started server at port 8080."
