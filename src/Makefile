# System makefile

export ERL_LIBS=$(shell pwd):/usr/lib

APPS := $(wildcard hordad_*) contrib
REL_FILE := hordad.rel

.PHONY: apps $(APPS) emake clean boot

apps: $(APPS) boot

emake:
	@for app in $(APPS); do \
		cd $$app && \
		make emake && \
		erl -make && \
		cd ..; \
	 done

$(APPS):
	@export ERL_LIBS=$(ELIBS)
	make -C $@

boot:
	./tools/rel_versions.py ./$(REL_FILE)
	erl -noinput -eval \
	'systools:make_script("hordad", [local, {path, ["*/ebin"]}]), halt(0).'

clean:
	@rm -f hordad.boot hordad.script
	@find . -name erl_crash.dump -delete
	@for app in $(APPS); do \
	   make -C $$app clean; \
	done