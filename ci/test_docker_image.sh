#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

network="amoc-arsenal-test-network"
path_to_exec="/home/amoc/amoc_arsenal_xmpp/bin/amoc_arsenal_xmpp"
amoc_nodes="['amoc_arsenal_xmpp@amoc-arsenal-1']"
number_of_nodes=3

docker network create "${network}"

for i in $(seq 1 "$number_of_nodes"); do
  name="amoc-arsenal-$i"
  docker run -td --rm -e AMOC_NODES="${amoc_nodes}" \
             --name "$name" -h "$name" --network "${network}" \
             --health-cmd="\"${path_to_exec}\" status" \
             amoc-arsenal-xmpp:latest
done

for i in $(seq 1 "$number_of_nodes"); do
  name="amoc-arsenal-$i"
  ./ci/wait_for_healthcheck.sh "$name"
done

for i in $(seq 1 "$number_of_nodes"); do
  name="amoc-arsenal-$i"
  output="$(docker exec -t "$name" "$path_to_exec" eval "nodes().")"
  echo  "container == '${name}', nodes() == ${output}"
  for j in $(seq 1 "$number_of_nodes"); do
    if [ "$j" -ne "$i" ]; then
      node_name="amoc_arsenal_xmpp@amoc-arsenal-$j"
      echo "$output" | grep -q "$node_name"
    fi
  done
done
