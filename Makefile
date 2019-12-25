.PHONY: test docs

test:
	lein test
	clj -A:test -C:test -e "(do (require 'clojure.test) (clojure.test/run-all-tests))"

docs:
	rm -rf docs
	lein codox
	cp -r target/doc docs/
