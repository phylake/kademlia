
    # build server
    ghc -O2 -threaded -rtsopts -eventlog UDPServer

    # run server
    ./UDPServer +RTS -N -A500M -la
    
    # build client
    ghc -O2 -threaded -rtsopts UDPClient

    # run client
    ./UDPClient +RTS -N -A500M


When the client finishes running you can stop the server and open
UDPServer.eventlog in threadscope
