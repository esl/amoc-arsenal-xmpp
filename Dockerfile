FROM phusion/baseimage:focal-1.0.0 as builder

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y git g++ make openssl libssl-dev gnupg2 wget

ARG otp_vsn=24.0-1

RUN wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb && \
    dpkg -i erlang-solutions_2.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:${otp_vsn}

ARG REBAR3_VERSION="3.16.1"
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

ARG amoc_arsenal_home=/home/amoc/amoc_arsenal_xmpp
RUN mkdir ${amoc_arsenal_home}
COPY --from=builder amoc_arsenal_build/_build/default/rel/amoc_arsenal_xmpp/ ${amoc_arsenal_home}
RUN chown -R amoc:amoc ${amoc_arsenal_home}

RUN mkdir /etc/service/amoc
## this env is required for amoc.sh script
ENV EXEC_PATH ${amoc_arsenal_home}/bin/amoc_arsenal_xmpp
COPY --from=builder amoc_arsenal_build/_build/default/lib/amoc/docker/amoc.sh /etc/service/amoc/run

EXPOSE 4000

CMD ["/sbin/my_init"]
