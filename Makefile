.PHONY: build clean dump-th ghci haddock haddock-server hlint lint release repl run test watch watch-tests watch-test
all: build

build:
	stack build

clean:
	stack clean

devel:
	stack exec -- yesod devel

# dump the template haskell
# (you must have a compile error in the code you want to dump)
dump-th:
	stack build --ghc-options="-ddump-splices"

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

# Watch for changes.
watch:
	stack build --file-watch --fast .

# Watch for changes.
watch-test: watch-tests
watch-tests:
	stack test --file-watch --fast .
