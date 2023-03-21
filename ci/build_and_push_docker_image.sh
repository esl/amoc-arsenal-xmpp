#!/bin/bash

export otp_vsn="${OTP_RELEASE:-25.3}"
echo "ERLANG/OTP ${otp_vsn}"

docker buildx build --platform linux/amd64,linux/arm64 \
       --build-arg otp_vsn \
       --push -t mongooseim/amoc-arsenal-xmpp:latest .
