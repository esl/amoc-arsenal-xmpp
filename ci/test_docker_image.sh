#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

cd "$(git rev-parse --show-toplevel)/ci"
docker compose up --wait --wait-timeout 100 --scale amoc-worker=2

function assert_equal()
{
  local entity=$1
  local expected_value=$2
  local received_value=$3
  [ "$expected_value" = "$received_value" ] \
    || { echo -e "invalid ${entity}\n\texpected: '${expected_value}'\n\treceived: '${received_value}'";
         exit 1; }
}

function assert_match()
{
  local entity=$1
  local pattern=$2
  local value=$3
  ( echo "$value" | grep -qxE -e "$pattern" ) \
    || { echo -e "${entity} doesn't match the pattern:\n\tpattern: '${pattern}'\n\tvalue:   '${value}'";
         exit 1; }
}

function get_nodes()
{
  curl -s -X GET "http://localhost:4000/nodes" -H  "accept: application/json" \
    | jq --compact-output '.nodes | keys' \
    | gsed -nE 's/[^"]*"([^"]+)"[^"]*/\1\n/gp'
}

function number_of_nodes()
{
  curl -s -X GET "http://localhost:4000/nodes" -H  "accept: application/json" \
    | jq '.nodes | length'
}

assert_equal "number of nodes" 3 "$(number_of_nodes)"

for node in $(get_nodes); do
  assert_match "node name" "amoc_arsenal_xmpp@.*" "$node"
done
