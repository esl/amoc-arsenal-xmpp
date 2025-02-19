#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)/ci"
docker compose --profile with_prometheus up --wait \
       --wait-timeout 100 --scale amoc-worker=2
