#!/bin/bash

# Get current repo version
version="$(git rev-parse --short HEAD)"
otp_vsn="${OTP_RELEASE:-24.0}-1"
echo "ERLANG/OTP ${otp_vsn}"
echo "AMOC-ARSENAL-XMPP ${version}"

docker build \
	-f Dockerfile \
	-t "amoc-arsenal-xmpp:${version}" \
	-t "amoc-arsenal-xmpp:latest" \
	--build-arg otp_vsn="${otp_vsn}" \
	.
