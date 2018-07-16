# crc32cer - CRC32-C Erlang NIF

This is a clone of [https://github.com/edenhill/librdkafka](https://github.com/edenhill/librdkafka) with only crc32-c related code retained.

Then added Erlang NIF support.

## Usage

```
crc32c:nif(Iodata).
crc32c:nif(Acc, Iodata).
```

