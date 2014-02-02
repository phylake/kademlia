Reproducing these results
=========================

    ghc -O2 -threaded UDPServer
    ./UDPServer 3001
    ./UDPServer 3002
    
    ghc -O2 -threaded -rtsopts UDPClient
    ./UDPClient -o UDPClient.html +RTS -A500M
