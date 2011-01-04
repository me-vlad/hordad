# System makefile
#
# Max E. Kuznecov <mek@mek.uz.ua>
#
# Possible targets:
# all: Build using regular make
# emake: Build using emake (significantly faster)
# typecheck: Run dialyzer static type analysis checks
# tests: Run application tests 

export ERL_LIBS=$(shell pwd):$(shell pwd)/../lib

START_TESTS = ./start-tests.sh
APPS := $(wildcard hordad_*) contrib
REL_FILE := hordad.rel

.PHONY: all $(APPS) emake doemake clean boot typecheck tests

all: $(APPS) boot

emake: doemake boot

doemake:
	@for app in $(APPS); do \
		cd $$app && \
		make emake && \
		cd ..; \
	 done

typecheck: $(APPS) boot
	dialyzer -r hordad_*

tests: $(APPS) boot
	@for app in $(APPS); do \
		$(START_TESTS) $$app; \
	done

$(APPS):
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

