Results
=======

    benchmarking udp/connect and send
    time                 3.125 μs   (3.116 μs .. 3.140 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 3.134 μs   (3.125 μs .. 3.146 μs)
    std dev              34.36 ns   (26.16 ns .. 48.03 ns)

    benchmarking udp/sendto
    time                 3.654 μs   (3.642 μs .. 3.668 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 3.644 μs   (3.633 μs .. 3.657 μs)
    std dev              41.19 ns   (32.76 ns .. 55.52 ns)

Reproducing these results
=========================

    cabal install
    UDPServer 3001 &
    UDPServer 3002 &
    UDPClient -o UDPClient.html
