Reproducing these results
=========================

    ghc -O2 -threaded UDPServer
    ./UDPServer 3001
    ./UDPServer 3002
    
    ghc -O2 -threaded -rtsopts UDPBench
    ./UDPBench -o UDPBench.html +RTS -A500M
