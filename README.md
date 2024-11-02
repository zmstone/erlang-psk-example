# psker

A demo application to run TLS/DTLS client & server using PSK (PreShared Key)

## Start server

```
$ export PSKER_START=server
./server.sh
```

## Start client

```
$ export PSKER_START=client
./client.sh
```

## Other environment variables

### For client

* `PSKER_SERVER_HOST`: default localhost, used to specify server hostname
* `PSKER_CIPHER`: set `rsa` to use RSA cipher suites, `non-rsa` for non-RSA cipher suites, or a specific one e.g. `RSA-PSK-AES256-GCM-SHA384` or `PSK-AES256-GCM-SHA384`.

### For both server and client

* `PSKER_SERVER_PORT`: default 9999, used to specify server port number
* `PSKER_PROTOCOL`: set to `dtls` or `tls` (default)
