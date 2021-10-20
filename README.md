# psker

A demo application to run TLS/DTLS client & server using PSK (PreShared Key)

## Start server

```
$ export PSKER_START=server
$ rebar3 shell
```

## Start client

```
$ export PSKER_START=client
$ rebar3 shell
```

## Other environment variables

### For client

* `PSKER_SERVER_HOST`: default localhost, used to specify server hostname

### For both server and client

* `PSKER_SERVER_PORT`: default 9999, used to specify server port number
* `PSKER_PROTOCOL`: set to `dtls` or `tls` (default)
