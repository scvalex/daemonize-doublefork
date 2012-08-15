.PHONY: all build dist install test clean doc

all: build

build: dist/setup-config
	cabal build

dist: build
	cabal sdist

install: build
	cabal install

clean:
	cabal clean

dist/setup-config: daemonize-doublefork.cabal
	cabal configure --enable-tests

doc: build
	cabal haddock
