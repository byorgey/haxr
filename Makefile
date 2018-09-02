##################################################
default: build

.PHONY: default

##################################################
check:
	cabal new-build -fno-code -O0 all

.PHONY: check

##################################################
repl:
	cabal new-repl haxr

.PHONY: repl

##################################################
clean:
	rm -rf "dist/" "dist-newstyle/"
	rm -f *.project.local .ghc.environment.*

.PHONY: clean

##################################################
build: cabal-compile

.PHONY: build

##################################################
cabal-compile:
	cabal new-build all

.PHONY: cabal-compile

##################################################
stack-compile:
	stack --nix build

.PHONY: stack-compile

##################################################
sdist: build
	cabal sdist

.PHONY: sdist

##################################################