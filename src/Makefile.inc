# Application-level makefile template

export ERL_LIBS=..

BEAMS: $(patsubst src/%.erl,ebin/%.beam, $(wildcard src/*.erl))

ERLC := erlc
ERLC_FLAGS := -W5
INCLUDE = -I ./include

.PHONY: all compile clean

all: compile

compile: $(BEAMS)

clean:
	@rm -f ebin/*.beam

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) $(INCLUDE) -o ./ebin/ $<