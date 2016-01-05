all: build

build: site
	stack exec site build

site: app/Site.hs
	stack setup
	stack build

new:
	@./new_post.sh

publish: clean build
	rsync -az --delete _site/ crydee:www/blog

clean:
	stack clean
	rm -rf _cache _site

.PHONY: all build site new publish clean
