#!/bin/sh
curl -X POST -H "Content-Type: application/n-triples" -d @`dirname $0`/task3.nt http://localhost:8080/tasks/ -v