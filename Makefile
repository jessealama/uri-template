.PHONY: clean test

all:
	# nothing to do

test:
	find . -mindepth 1 -maxdepth 1 -type f -name '*.rkt' -exec basename {} ';' | parallel --jobs=1 --halt-on-error=now,fail=1 raco test {}

clean:
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	rm -Rf compiled
	find . -mindepth 1 -maxdepth 1 -type d ! -name '.git' -exec $(MAKE) --directory {} clean ';'
