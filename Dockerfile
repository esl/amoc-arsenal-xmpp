FROM phusion/baseimage AS builder
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN useradd -ms /bin/bash amoc

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        git \
        make \
        gcc \
        g++ \
        clang \
        libexpat1-dev \
        wget \
        iproute2 && \
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:21.2.2-1

#FROM mongooseim/amoc_builder_base AS builder
COPY . /amoc_arsenal_build

RUN cd amoc_arsenal_build && \
    ./rebar3 release

FROM phusion/baseimage
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

ARG vsn
ARG amoc_vsn
ARG amoc_arsenal_home=/home/amoc/amoc_arsenal_xmpp
ARG amoc_github=https://raw.githubusercontent.com/esl/amoc

ENV EXEC_PATH ${amoc_arsenal_home}/bin/amoc_arsenal_xmpp
ENV SYSCONFIG ${amoc_arsenal_home}/releases/${vsn}/sys.config
ENV VMARGS ${amoc_arsenal_home}/releases/${vsn}/vm.args

RUN useradd -ms /bin/bash amoc

RUN mkdir ${amoc_arsenal_home}
COPY --from=builder amoc_arsenal_build/_build/default/rel/amoc_arsenal_xmpp/ ${amoc_arsenal_home}
# It seems hub.docker.com does not support --chown param to COPY directive
RUN chown -R amoc:amoc ${amoc_arsenal_home}

EXPOSE 4000

RUN mkdir /etc/service/amoc
RUN curl ${amoc_github}/${amoc_vsn}/docker/amoc.sh --output /etc/service/amoc/run && chmod +x /etc/service/amoc/run
RUN curl ${amoc_github}/${amoc_vsn}/docker/config/vm.args --output /home/amoc/amoc_arsenal_xmpp/releases/${vsn}/vm.args
RUN curl ${amoc_github}/${amoc_vsn}/docker/config/sys.config.template --output /sys.config.template
RUN curl ${amoc_github}/${amoc_vsn}/docker/run.sh --output /run.sh && chmod +x /run.sh

CMD ["/run.sh"]
