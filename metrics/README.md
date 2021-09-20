to start up influxdb and grafana run this command:
```
docker-compose up -d
```

after that configure influxdb data source for grafana:
```
curl 'http://admin:admin@localhost:3000/api/datasources' \
     -X POST -H 'Content-Type: application/json;charset=UTF-8' \
     -d '{"name": "InfluxDB", "access": "proxy", "type": "influxdb",
          "url": "http://influxdb:8086", "isDefault": true, 
          "database": "graphite"}'
```

---

use this command for troubleshooting and [exploration of influxdb schema](https://docs.influxdata.com/influxdb/v1.8/query_language/explore-schema/):
```
docker-compose exec influxdb influx
```

---

MongooseIM reporter configuration should have the following format (configured in the `app.config` file):
```
{exometer_core, [
    {mongooseim_report_interval, 10000}, %% 10 seconds
    {report, [
        {reporters, [
            {exometer_report_graphite, [
                {prefix, {{{ instance_id }}} ".mongoose"},
                {host, "10.251.192.82"},
                {api_key, ""}
            ]}
        ]}
    ]}
]}
```

where `instance_id` is some unique identifier w/o dots, e.g. the value returned by `hostname -s` command.

---

you may want to collect generic metrics (e.g. mem/cpu utilisation, network statistics, disk IO) from the
machines where MIM/amoc containers are running. [Telegraf](https://docs.influxdata.com/telegraf/v1.19/)
is a good tool for doing this. here is an example of `telegraf.conf` file:
```
[global_tags]

##---------------------------------------------------------------------------------------------
## https://docs.influxdata.com/telegraf/v1.19/administration/configuration/#agent-configuration
##---------------------------------------------------------------------------------------------
[agent]
  interval = "10s"
  round_interval = true
  metric_batch_size = 1000
  metric_buffer_limit = 10000
  collection_jitter = "0s"
  flush_interval = "10s"
  flush_jitter = "0s"
  precision = ""
  debug = false
  quiet = false
  logfile = ""
  hostname = ""
  omit_hostname = true

##--------------------------------------------------------------------------------------------
## https://github.com/influxdata/telegraf/blob/release-1.19/plugins/outputs/graphite/README.md
##--------------------------------------------------------------------------------------------
[[outputs.graphite]]
  servers = ["localhost:2003"]
  ## Prefix metrics name should follow this format:
  # prefix = "telegraf.amoc.<short_host_name_without_dots>"
  # prefix = "telegraf.mongooseim.<short_host_name_without_dots>"
  prefix = "telegraf.amoc.amoc_host_machine"
  template = "tags.measurement.field"
  timeout = 2

##------------------------------------------------------------------
## https://docs.influxdata.com/telegraf/v1.19/plugins/#input-plugins
##------------------------------------------------------------------
[[inputs.cpu]]
  percpu = true
  totalcpu = true
  report_active = true
[[inputs.disk]]
[[inputs.diskio]]
  skip_serial_number = true
[[inputs.mem]]
[[inputs.net]]
[[inputs.netstat]]
```
