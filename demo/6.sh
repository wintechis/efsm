#!/bin/sh
curl -X PUT -H "Content-Type: application/n-triples" -d @task4.nt http://localhost:8080/tasks/4 -v