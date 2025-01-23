build:
	elm make src/Main.elm --output=./build/main.js
	cp index.html build/

run:
	cd build && npx live-server

build-run: build run

clean:
	rm -rf elm-stuff/
	rm -rf build/

.PHONY: build