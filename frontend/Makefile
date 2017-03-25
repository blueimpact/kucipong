.PHONY: build clean yarn-install watch
all: build

build: yarn-install
	yarn run build

clean:
	rm -rf dist
	rm -rf elm-stuff

yarn-install:
	yarn install

watch: yarn-install
	yarn start

