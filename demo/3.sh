#!/bin/sh
curl -X PUT -H "Content-Type: application/n-triples" -d @`dirname $0`/vars1.nt http://localhost:8080/ -v