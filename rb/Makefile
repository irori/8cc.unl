all: core16.unl core24.unl

core16.unl:
	gosh ../unlcore.scm --generate-core >$@

core24.unl:
	BITLENGTH=24 gosh ../unlcore.scm --generate-core >$@
