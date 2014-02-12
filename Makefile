all: build

build: site
	./site build

site: site.hs
	ghc --make site.hs
	./site clean

new:
	@./new_post.sh

publish: clean build
	rsync -az --delete _site/ ~/server/www/blog

clean:
	test -f site && ./site clean || true
	rm -f site site.hi site.o

.PHONY: all build new publish clean
