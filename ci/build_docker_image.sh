#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

# Get current repo version
version="$(git rev-parse --short HEAD)"
otp_vsn="${OTP_RELEASE:-25.3}"
echo "ERLANG/OTP ${otp_vsn}"
echo "AMOC-ARSENAL-XMPP ${version}"

docker build \
	-f Dockerfile \
	-t "amoc-arsenal-xmpp:${version}" \
	-t "amoc-arsenal-xmpp:latest" \
	--build-arg otp_vsn="${otp_vsn}" \
	.
