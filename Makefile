.PHONY: build-docs
build-docs:
	cd docbuild; lake build HaskellSpec:docs

.PHONY: serve-docs
serve-docs: build-docs
	cd docbuild/.lake/build/doc/; python3 -m http.server

.PHONY: build-book
build-book:
	cd book-report; lake exe report --output _out/html --depth 2

.PHONY: serve-book
serve-book: build-book
	cd book-report/_out/html/html-multi/; python3 -m http.server