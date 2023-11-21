ARG otp_vsn=25.3
FROM erlang:${otp_vsn}
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN apt-get update
RUN apt-get install -y host

WORKDIR /amoc_arsenal_xmpp
COPY ./ ./

## build only what is commited
RUN git clean -ffxd
RUN git restore -WS .

RUN rebar3 release

ENV PATH "/amoc_arsenal_xmpp/_build/default/rel/amoc_arsenal_xmpp/bin:${PATH}"

CMD ["amoc_arsenal_xmpp", "console", "-noshell", "-noinput", "+Bd"]
