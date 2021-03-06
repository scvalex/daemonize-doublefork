.PHONY: all build dist install clean doc

all: build

build: dist/setup-config
	cabal build

dist:
	cabal sdist

install: build
	cabal install

clean:
	cabal clean

dist/setup-config: daemonize-doublefork.cabal
	cabal configure

doc: build
	cabal haddock
