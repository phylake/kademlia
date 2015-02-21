Results
=======

[HTML]()

Raw

```
benchmarking zmq PUB-SUB/only
time                 230.7 ns   (227.7 ns .. 234.6 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 237.1 ns   (234.8 ns .. 239.3 ns)
std dev              7.711 ns   (6.155 ns .. 9.677 ns)
variance introduced by outliers: 48% (moderately inflated)
```

Reproducing these results
=========================

Notice this is the opposite invocation order as REQ_REP because the PUB/SUB
needs to synchronize. Also you can omit `-L. -optl-Wl,-rpath,'$ORIGIN'`
if your ghc is smart enough to find libzmq.so

    # build client benchmark
    ghc -O2 -threaded -rtsopts -lzmq -L. -optl-Wl,-rpath,'$ORIGIN' ZMQClient

    # start client benchmark
    ./ZMQClient -o ZMQClient.html +RTS -N1 -A20M

    # build server
    ghc -O2 -threaded -rtsopts -lzmq -L. -optl-Wl,-rpath,'$ORIGIN' ZMQServer
    
    # start server
    ./ZMQServer +RTS -N1 -A20M
