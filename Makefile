build-docs:
	cd docbuild; lake build HaskellSpec:docs

serve-docs: build-docs
	cd docbuild/.lake/build/doc/; python3 -m http.server