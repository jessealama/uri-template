.PHONY: clean test

all:
	# nothing to do

test:
	raco test *.rkt

clean:
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	rm -Rf compiled
	find . -mindepth 1 -maxdepth 1 -type d ! -name '.git' -exec $(MAKE) --directory {} clean ';'
