build:
	elm make src/Main.elm --output=main.js

run:
	npx http-server .

clean:
	rm -rf elm-stuff/
	rm -rf main.js

build-run: build run

