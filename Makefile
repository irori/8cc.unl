LIBF_URL := https://rawgit.com/shinh/bflisp/66eefd11889d6ea37225f0060b775b6a44133461/libf.h

$(shell mkdir -p out)

all: out/8cc.unl unlambda/unlambda out/hello.unl

check: unlambda/unlambda out/hello.unl
	unlambda/unlambda out/hello.unl

out/%.unl: test/%.bfs
	gosh unlcore.scm < $< > $@

unlambda/unlambda: unlambda/unlambda.c out/git_submodule.stamp
	$(MAKE) -C unlambda

out/libf.h:
	wget $(LIBF_URL) -O $@.tmp && mv $@.tmp $@

out/8cc.c: merge_8cc.sh out/libf.h 8cc/README.md
	./merge_8cc.sh > $@.tmp && mv $@.tmp $@

out/8cc.bfs: out/8cc.c 8cc/8cc
	8cc/8cc -S -o $@.tmp $< && mv $@.tmp $@

out/8cc.unl out/8cc.2.unl out/8cc.3.unl: out/8cc%.unl: out/8cc%.bfs
	BFS24=1 gosh unlcore.scm < $< > $@.tmp && mv $@.tmp $@

out/8cc.2.bfs: out/8cc.unl out/8cc.c unlambda/unlambda
	@echo
	@echo "generating $@..."
	@echo "Warning: this takes ~1.5 days and consumes more than 10GB RAM!"
	@echo
	unlambda/unlambda $< < out/8cc.c > $@.tmp && mv $@.tmp $@

out/8cc.3.bfs: out/8cc.2.unl out/8cc.c unlambda/unlambda
	@echo
	@echo "generating $@..."
	@echo "Warning: this takes ~1.5 days and consumes more than 10GB RAM!"
	@echo
	unlambda/unlambda $< < out/8cc.c > $@.tmp && mv $@.tmp $@

clean:
	rm -rf out

.PHONY: all check clean

8cc/8cc: $(wildcard 8cc/*.c 8cc/*.h) out/git_submodule.stamp
	$(MAKE) -C 8cc

out/git_submodule.stamp: .gitmodules
	git submodule update --init
	touch $@
