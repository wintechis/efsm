#!/bin/sh
curl -X PUT -H "Content-Type: application/n-triples" -d @task3.nt http://localhost:8080/tasks/3 -v