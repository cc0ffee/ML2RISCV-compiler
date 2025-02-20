all:
	dune build
	cp _build/default/src/main.exe main
	chmod 771 main

main: all

clean:
	rm -rf main
	dune clean

test: main
	./test.sh p5tests
