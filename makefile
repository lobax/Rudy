

erl_files=$(wildcard src/*.erl)
beam_files=$(patsubst src/%.erl, ebin/%.beam, $(erl_files))

ebin/%.beam: src/%.erl
	mkdir -p ebin
	erlc -o ebin $<

all: $(beam_files)

.PHONY: clean run

clean: 
	rm -rf ebin/

run:
	erl -pa ./ebin -eval 'rudy:start(8080)' 
