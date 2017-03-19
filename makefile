

erl_files=$(wildcard src/*.erl)
beam_files=$(patsubst src/%.erl, ebin/%.beam, $(erl_files))

ebin/%.beam: src/%.erl
	mkdir -p ebin
	erlc -o ebin $<

all: $(beam_files)

.PHONY: clean run rudy2

clean: 
	rm -rf ebin/
	rm -rf out/

run: all
	erl -pa ./ebin -eval 'rudy:start(8080)'

rudy2: all
	erl -pa ./ebin -eval 'rudy2:start(8080)'

test: all
	mkdir -p out
	@erl -pa ./ebin -eval 'test:bench(100)' -eval 'init:stop()' -noshell
	gnuplot gnuplot.conf

plot: output.dat
	gnuplot gnuplot.conf
