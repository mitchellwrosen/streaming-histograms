.PHONY: build
build:
	cabal new-build -O0

.PHONY: test
test:
	cabal new-test -O0
