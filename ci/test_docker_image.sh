#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

NETWORK=amoc-arsenal-test-network
PATH_TO_EXEC=/home/amoc/amoc_arsenal_xmpp/bin/amoc_arsenal_xmpp
AMOC_NODES="['amoc_arsenal_xmpp@amoc-arsenal-1','amoc_arsenal_xmpp@amoc-arsenal-1']"
docker network create ${NETWORK}

docker run -t -d --name amoc-arsenal-1 -h amoc-arsenal-1 --network ${NETWORK} \
           -e AMOC_NODES="${AMOC_NODES}" --health-cmd="${PATH_TO_EXEC} status" \
           amoc-arsenal-xmpp:latest

docker run -t -d --name amoc-arsenal-2 -h amoc-arsenal-2 --network ${NETWORK} \
           -e AMOC_NODES="${AMOC_NODES}" --health-cmd="${PATH_TO_EXEC} status" \
	       amoc-arsenal-xmpp:latest

./ci/wait_for_healthcheck.sh amoc-arsenal-1
./ci/wait_for_healthcheck.sh amoc-arsenal-2

docker exec -t amoc-arsenal-1 ${PATH_TO_EXEC} eval "nodes()" | grep amoc-arsenal-2
docker exec -t amoc-arsenal-2 ${PATH_TO_EXEC} eval "nodes()" | grep amoc-arsenal-1


