
build:
	stack build

clean:
	stack clean

run:
	stack exec stl-parser -- test.stl

.PHONY: build clean run

