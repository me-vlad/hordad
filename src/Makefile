# System makefile

APPS := $(wildcard hordad_*) contrib
REL_FILE := hordad.rel

.PHONY: apps $(APPS) clean boot

apps: $(APPS) boot

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