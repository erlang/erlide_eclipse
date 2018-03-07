.SUFFIXES:
.SUFFIXES: .erl .beam

.erl.beam: ; erlc -W $<

MODS = src/navigation

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump 
