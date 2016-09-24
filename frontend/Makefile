.PHONY: build clean npm-install watch
all: build

build: npm-install
	npm run build

clean:
	rm -rf dist

npm-install:
	if [ ! -d "node_modules" ] ; then \
		npm install ; \
	fi

watch: npm-install
	npm start

