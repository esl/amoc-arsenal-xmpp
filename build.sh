#!/bin/bash

# Get current repo version
version=$(git rev-parse --short HEAD)

# Get Amoc version
./rebar_lock_json rebar.lock > /tmp/$version-rebar.lock.json
amoc_version=$(python -c 'import json,sys;obj=json.load(sys.stdin);print obj["amoc"]["git_ref"]' < /tmp/$version-rebar.lock.json)

docker build \
	-f Dockerfile \
	-t amoc-arsenal-xmpp:latest \
	--build-arg vsn=$version \
	--build-arg amoc_vsn=$amoc_version \
	.
