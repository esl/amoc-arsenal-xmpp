pattern="amoc-worker-[0-9]+"
prefix="$(host $(hostname -i) | sed -nE "0,/.*(${pattern}).*/{s//\1/p}")"
export AMOC_GRAPHITE_PREFIX="\"${prefix}\""
amoc_arsenal_xmpp console -noshell -noinput +Bd
