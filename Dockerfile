
FROM phusion/baseimage:bionic-1.0.0 as base

FROM base AS builder

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get install -y gnupg2

ADD https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb /tmp/
RUN dpkg -i /tmp/erlang-solutions_2.0_all.deb
RUN apt-get update

## to get the list of all available versions for your distro run:
##     apt-cache policy esl-erlang
ARG otp_vsn=1:22.3-1
RUN apt-get install -y esl-erlang=${otp_vsn}

RUN  apt-get install -y git g++ openssl libssl-dev

COPY . /amoc_arsenal_build
WORKDIR /amoc_arsenal_build
RUN git clean -ffxd
RUN ./rebar3 release

FROM base
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
