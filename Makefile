check: out/unl out/hello.unl
	out/unl out/hello.unl

out/%.unl: test/%.bfs
	mkdir -p out
	gosh unlcore.scm < $< > $@

out/unl: unl/unl.c
	mkdir -p out
	$(CC) -O3 -o $@ $<

clean:
	rm -f out/*
