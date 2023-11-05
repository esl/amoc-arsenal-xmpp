ARG otp_vsn
FROM erlang:${otp_vsn} as builder

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y git g++ make openssl libssl-dev gnupg2 wget

ARG REBAR3_VERSION="3.20.0"
RUN set -xe \
    && REBAR3_DOWNLOAD_URL="https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" \
    && curl -fSL -o rebar3 "$REBAR3_DOWNLOAD_URL" \
    && install -v ./rebar3 /usr/local/bin/

COPY . /amoc_arsenal_build
WORKDIR /amoc_arsenal_build
RUN git clean -ffxd
RUN rebar3 release

FROM builder
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN useradd -ms /bin/bash amoc

USER amoc

ARG amoc_arsenal_home=/home/amoc/amoc_arsenal_xmpp
RUN mkdir ${amoc_arsenal_home}
COPY --from=builder amoc_arsenal_build/_build/default/rel/amoc_arsenal_xmpp/ ${amoc_arsenal_home}

EXPOSE 4000

CMD ["/home/amoc/amoc_arsenal_xmpp/bin/amoc_arsenal_xmpp", "console", "-noshell", "-noinput", "+Bd"]
