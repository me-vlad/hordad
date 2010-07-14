# Application-level makefile template

BEAMS: $(patsubst src/%.erl,ebin/%.beam, $(wildcard src/*.erl))

ERLC := erlc
ERLC_FLAGS := -W5
INCLUDE = -I ./include
EMAKEFILE = Emakefile

.PHONY: all compile clean emake

all: compile

compile: $(BEAMS)

emake:
	@cat /dev/null > $(EMAKEFILE)
	@echo "{'src/*', [{outdir, \"ebin\"}, {i, \"./include\"}, warn_export_all, warn_export_vars, warn_obsolete_guard, warn_unused_import]}." \
	>> $(EMAKEFILE)

clean:
	@rm -f ebin/*.beam
	@rm -f $(EMAKEFILE)

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) $(INCLUDE) -o ./ebin/ $<
