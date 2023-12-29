#! /usr/bin/env bash

set -euo pipefail

cd mate

# Get all environments
envs=$(pio project config --json-output \
           | jq -r '.[] | .[0] | select(startswith("env:")) | ltrimstr("env:")')

# Build each environment
for env in $envs; do
    echo "Building $env"
    pio run --environment "$env"
done

exit 0
