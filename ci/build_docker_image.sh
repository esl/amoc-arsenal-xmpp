#!/bin/bash

# Get current repo version
version="$(git rev-parse --short HEAD)"
otp_vsn="1:${OTP_RELEASE:-22.3}-1"
echo "ERLANG/OTP ${otp_vsn}"
echo "AMOC-ARSENAL-XMPP ${version}"

docker build \
	-f Dockerfile \
	-t "amoc-arsenal-xmpp:${version}" \
	-t "amoc-arsenal-xmpp:latest" \
	--build-arg otp_vsn="${otp_vsn}" \
	.
