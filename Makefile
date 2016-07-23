.PHONY: default develop test

default: develop

develop:
	docker build -t kademlia-dev .

	docker run -it --rm \
	-v $$PWD:/host \
	-w /host \
	kademlia-dev

test:
	cabal configure --enable-tests
	cabal build -j
	cabal test -j --show-details=streaming
