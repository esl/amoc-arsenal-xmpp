## Metrics

`amoc_metrics` allow to configure a Graphite reporter using the following environment variables:

* ``graphite_host`` - a graphite host address (string or `undefined`):
    * default value - `undefined` (amoc_metrics do not try to initialise a metrics reporter)
    * example: `AMOC_GRAPHITE_HOST='"graphite"'`

* ``graphite_port`` - graphite port:
    * default value - `2003`
    * example: `AMOC_GRAPHITE_PORT='2003'`

* ``graphite_prefix`` - graphite prefix:
    * default value - `net_adm:localhost()`
    * example: `AMOC_GRAPHITE_PREFIX='"amoc"'`

In order to initialise some preconfigured metrics, other applications can declare the `predefined_metrics` environment variable (in their own `*.app.src` file):  
```erl
{predefined_metrics, [{gauge, some_metric}, {times, another_metric}]}
```
