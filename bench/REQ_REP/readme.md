Results
=======

    warming up
    estimating clock resolution...
    mean is 1.620542 us (320001 iterations)
    found 929 outliers among 319999 samples (0.3%)
      656 (0.2%) high severe
    estimating cost of a clock call...
    mean is 51.49624 ns (12 iterations)
    found 1 outliers among 12 samples (8.3%)
      1 (8.3%) high severe

    benchmarking zmq REQ-REP/connect each time
    mean: 2.271327 ms, lb 2.186972 ms, ub 2.372762 ms, ci 0.950
    std dev: 473.4057 us, lb 397.9564 us, ub 613.0563 us, ci 0.950
    found 3 outliers among 100 samples (3.0%)
      2 (2.0%) high mild
      1 (1.0%) high severe
    variance introduced by outliers: 94.675%
    variance is severely inflated by outliers

    benchmarking zmq REQ-REP/connect once
    mean: 508.1328 us, lb 482.6190 us, ub 565.4701 us, ci 0.950
    std dev: 186.2112 us, lb 105.5652 us, ub 375.7306 us, ci 0.950
    found 1 outliers among 100 samples (1.0%)
      1 (1.0%) high severe
    variance introduced by outliers: 98.886%
    variance is severely inflated by outliers

Reproducing these results
=========================

Notice this is the opposite invocation order as PUB_SUB. Also you can omit
`-L. -optl-Wl,-rpath,'$ORIGIN'` if your ghc is smart enough to find
libzmq.so

    # build server
    ghc -O2 -threaded -rtsopts -lzmq -L. -optl-Wl,-rpath,'$ORIGIN' ZMQServer

    # start server on 3000
    ./ZMQServer 3000 +RTS -N -A500M

    # start server on 3001
    ./ZMQServer 3001 +RTS -N -A500M

    # build client benchmark
    ghc -O2 -threaded -rtsopts -lzmq -L. -optl-Wl,-rpath,'$ORIGIN' ZMQClient
    
    # start client benchmark
    ./ZMQClient -o ZMQClient.html +RTS -N -A500M
