ARG otp_vsn=25.3
FROM erlang:${otp_vsn} as builder
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

WORKDIR /amoc_arsenal_xmpp

COPY ./rebar.config ./rebar.lock ./
RUN rebar3 deps && rebar3 compile -d

COPY ./rel rel
COPY ./src src
RUN rebar3 release

ENV PATH "/amoc_arsenal_xmpp/_build/default/rel/amoc_arsenal_xmpp/bin:${PATH}"

CMD ["/amoc_arsenal_xmpp/_build/default/rel/amoc_arsenal_xmpp/bin/amoc_arsenal_xmpp", "console", "-noshell", "-noinput", "+Bd"]
