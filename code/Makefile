_build/default/src/bin/tree_sampler.exe: 
	dune build src/bin/tree_sampler.exe --root=.

tree_sampler: _build/default/src/bin/tree_sampler.exe
	sudo cp $< $@

clean: 
	dune clean --root=.
	rm -f ./tree_sampler

build:
	dune build src/bin/tree_sampler.exe --root=.

rebuild: clean gossip
	