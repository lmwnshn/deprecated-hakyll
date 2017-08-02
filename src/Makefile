.PHONY: clean build watch

clean:
	stack exec site clean

build:
	stack build --fast
	stack build --fast --pedantic --haddock --test --no-run-tests --no-haddock-deps
	stack exec site rebuild

watch: build
	stack exec site watch
