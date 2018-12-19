.PHONY: test

test:
	lein test
	clj -A:test -C:test -e "(do (require 'clojure.test) (clojure.test/run-all-tests))"
