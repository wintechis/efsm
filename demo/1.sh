#!/bin/sh
curl -X PUT -H "Content-Type: application/n-triples" -d @task1.nt http://localhost:8080/tasks/1 -v