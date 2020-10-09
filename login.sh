#!/usr/bin/env bash
PORT=${1:-8080}
curl -v "http://localhost:${PORT}/~/login" \
    --data-raw 'password=lidlut-tabwed-pillex-ridrup&redirect=%2F' \
