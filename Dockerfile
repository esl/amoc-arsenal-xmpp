ARG otp_vsn=27.1
FROM erlang:${otp_vsn}
LABEL org.label-schema.name='AMOC Arsenal' \
      org.label-schema.vendor='Erlang Solutions'

WORKDIR /amoc_arsenal_xmpp

COPY rebar.lock .
RUN rebar3 compile --deps_only

COPY rebar.config .
COPY rel rel
COPY src src
RUN rebar3 release

ENV PATH="/amoc_arsenal_xmpp/_build/default/rel/amoc_arsenal_xmpp/bin:${PATH}"

CMD ["amoc_arsenal_xmpp", "console", "-noshell", "-noinput", "+Bd"]
