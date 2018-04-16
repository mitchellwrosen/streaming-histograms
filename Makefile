.PHONY: build
build:
	cabal new-build -O0

.PHONY: test
test:
	cabal new-test -O0

.PHONY: repl
repl:
	cabal new-repl -O0 streaming-histograms

.PHONY: docs
docs:
	cabal new-haddock -O0
