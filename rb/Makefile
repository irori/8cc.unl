all: core16.unl core24.unl unlcore.h

unlcore.h: core24.unl genhdr.rb
	./genhdr.rb core24.unl >unlcore.h

core16.unl: ../*.scm
	gosh ../unlcore.scm --generate-core >$@

core24.unl: ../*.scm
	BITLENGTH=24 gosh ../unlcore.scm --generate-core >$@
