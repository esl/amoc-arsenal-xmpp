ARG otp_vsn=25.3
FROM erlang:${otp_vsn}
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

WORKDIR /amoc_arsenal_xmpp
COPY ./ ./

## build only what is commited
RUN git clean -ffxd
RUN git restore -WS .

## if this command fails, run `rebar3 get-deps` before building image.
## this will ensure that amoc-arsenal is fetched into the docker build
## context directory, so it can be copied while building docker image.
COPY _build/default/lib/amoc_arsenal/ /Users/denysgonchar/git/amoc-arsenal

RUN rebar3 release

ENV PATH "/amoc_arsenal_xmpp/_build/default/rel/amoc_arsenal_xmpp/bin:${PATH}"

COPY --chmod=500 <<-EOF /start_amoc.sh
	amoc_arsenal_xmpp console -noshell -noinput +Bd
EOF

CMD ["sh", "/start_amoc.sh"]
