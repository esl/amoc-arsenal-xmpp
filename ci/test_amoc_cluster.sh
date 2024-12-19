#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

function assert_equal()
{
  local entity=$1
  local expected_value=$2
  local received_value=$3
  [ "$expected_value" = "$received_value" ] \
    || { echo "invalid ${entity}:";
         echo "   expected: '${expected_value}'";
         echo "   received: '${received_value}'";
         exit 1; }
}

function assert_match()
{
  local entity=$1
  local pattern=$2
  local value=$3
  ( echo "$value" | grep -qxE -e "$pattern" ) \
    || { echo "${entity} doesn't match the pattern:";
         echo "   pattern: '${pattern}'";
         echo "   value:   '${value}'";
         exit 1; }
}

function contains()
{
  local output="$(cat -)"
  local ret= acc=0
  for pattern in "$@"; do
      ret="$(echo "$output" | grep -q -e "$pattern"; echo "$?")"
      if [ "$ret" -ne "0" ]; then
          [ "$(($acc))" -eq "0" ] && {
              echo "contains FAILED"
              echo "output: '${output}'"; }
          echo "pattern is missing: '${pattern}'"
      fi >&2
      acc+="+${ret}"
  done
  test "$(($acc))" "-eq" "0"
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

function retry()
{
  local n="$1" m="0"
  shift 1
  echo "waiting for '$@'"
  until echo -n "." && "$@"; do
    [ "$n" -gt "$m" ] || { echo -e "\nfailed after '$m' retries"; return 1; }
    sleep 1
    m="$(( m + 1 ))"
  done
  echo -e "\nsuccess after '$m' retries";
}

function metrics_reported()
{
  curl -s 'http://localhost:9090/api/v1/targets' | contains "$@"
}

function wait_for_metrics() {
  retry 60 metrics_reported "$@"
}

assert_equal "number of nodes" 3 "$(number_of_nodes)"

for node in $(get_nodes); do
  assert_match "node name" "amoc_arsenal_xmpp@.*" "$node"
  wait_for_metrics "amoc-master"
done
