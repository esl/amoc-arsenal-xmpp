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

function get_graphite_prefix()
{
  local node="$1"
  curl -s -X GET "http://localhost:4000/status/$node" -H  "accept: application/json" \
    | jq '.env.AMOC_GRAPHITE_PREFIX | ltrimstr("\"") | rtrimstr("\"")' | sed 's/"//g'
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

function are_metrics_reported()
{
  local graphite_prefix="$1"
  local length="$(curl -s "http://localhost:8080/metrics/find?query=${graphite_prefix}.*" | jq "length")"
  [ "$length" -gt "0" ]
}

function wait_for_reported_metrics()
{
  local graphite_prefix="$1"
  retry 60 are_metrics_reported "$graphite_prefix"
}

assert_equal "number of nodes" 3 "$(number_of_nodes)"

for node in $(get_nodes); do
  assert_match "node name" "amoc_arsenal_xmpp@.*" "$node"
  if [ "$node" != "amoc_arsenal_xmpp@amoc-master" ]; then
    graphite_prefix="$(get_graphite_prefix $node)"
    wait_for_reported_metrics "$graphite_prefix"
  else
    wait_for_reported_metrics "amoc-master"
  fi
done
