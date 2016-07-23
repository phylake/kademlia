FROM haskell:7.10

RUN apt-get update
RUN apt-get install -y make

COPY kademlia.cabal /cabal_install/
COPY cabal.config /cabal_install/
COPY LICENSE /cabal_install/
WORKDIR /cabal_install
RUN cabal update
RUN cabal install -j --only-dependencies --enable-tests

CMD /bin/bash
