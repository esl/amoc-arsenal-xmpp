#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

export otp_vsn="${OTP_RELEASE:-27.3}"
echo "ERLANG/OTP ${otp_vsn}"

docker buildx build --platform linux/amd64,linux/arm64 \
       --target prod --push --build-arg otp_vsn \
       -t mongooseim/amoc-arsenal-xmpp:latest .
