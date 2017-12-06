
build:
	stack build

clean:
	stack clean

run:
	stack exec stl-parser

.PHONY: build clean run

