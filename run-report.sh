#!/usr/bin/env bash
set -e

cd "$(dirname "$0")"

# Run the container to completion
docker compose up

# Open the folder where the report lands
xdg-open "$(pwd)/tmp/NeonatalErrorReport.csv" &
