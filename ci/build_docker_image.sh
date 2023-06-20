#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "$git_root"

version="$(git rev-parse --short HEAD)"
otp_vsn="${OTP_RELEASE:-25.3}"
echo "ERLANG/OTP '${otp_vsn}'"

docker_compose build --build-arg "otp_vsn=${otp_vsn}"
