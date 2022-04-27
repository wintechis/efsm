module Main where

import Prelude

import Effect.Console (log)
import HTTPure (Request, ResponseM, ServerM, header, ok, ok', serve)

router :: Request -> ResponseM
router { path: [] } = ok "Bla"
router { path: ["hi", "ho"] } = ok' (header "Content-Type" "text/turtle") "<> a <http://example.org/Bla> ."
router _ = ok "default"

main :: ServerM
main = serve 8080 router $ log "Started server at port 8080."
