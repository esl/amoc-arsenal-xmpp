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