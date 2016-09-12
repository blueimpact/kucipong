.PHONY: build build-frontend build-server clean clean-frontend clean-server dump-th ghci haddock haddock-server hlint lint release repl run test watch watch-tests watch-test
all: build

build-frontend:
	$(MAKE) -C frontend build

build-server:
	stack build kucipong

build: build-frontend build-server

clean-frontend:
	$(MAKE) -C frontend clean

clean-server:
	stack clean

clean: clean-frontend clean-server

devel:
	stack exec -- yesod devel

# dump the template haskell
# (you must have a compile error in the code you want to dump)
dump-th:
	stack build --ghc-options="-ddump-splices"
	@echo
	@echo "Splice files:"
	@echo
	@find "$$(stack path --dist-dir)" -name "*.dump-splices"

ghci:
	stack ghci

haddock:
	stack build --haddock

# This runs a small python websever on port 8001 serving up haddocks for
# packages you have installed.
#
# In order to run this, you need to have run `make build-haddock`.
haddock-server:
	cd "$$(stack path --local-doc-root)" && python -m http.server 8001

hlint: lint

lint:
	hlint src/

# Start a repl.
repl: ghci

release:
	stack exec -- yesod keter
	scp kucipong.keter kucipong:
	ssh -t kucipong 'sudo cp kucipong.keter /var/www/keter/incoming/'

run: build
	stack exec -- kucipong Development

test:
	stack test

watch-frontend:
	$(MAKE) -C frontend watch

# Watch for changes.
watch-server:
	stack build --file-watch --fast kucipong

# TODO: Figure out how to watch on both frontend and server.
watch: watch-server

# Watch for changes.
watch-test: watch-tests
watch-tests:
	stack test --file-watch --fast .
