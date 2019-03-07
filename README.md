network-bitcoin
====

See the [Hackage documentation](http://hackage.haskell.org/package/network-bitcoin).

Testing
----

The tests expect to run against a `bitcoind` node running in regtest mode.
Invoke `bitcoind` with:

```shell
$ bitcoind -regtest -rpcuser=bitcoinrpc -rpcpassword=bitcoinrpcpassword -rpcport=18444
```
