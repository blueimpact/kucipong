.PHONY: build clean yarn-install watch
all: build

build: yarn-install
	yarn run build

clean:
	rm -rf dist

yarn-install:
	yarn install

watch: yarn-install
	yarn start

