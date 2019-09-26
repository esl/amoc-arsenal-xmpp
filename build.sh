#!/bin/bash

# Get current repo version
version=$(git rev-parse --short HEAD)

docker build \
	-f Dockerfile \
	-t amoc-arsenal-xmpp:latest \
	--build-arg vsn=$version \
	.
