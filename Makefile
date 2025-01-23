build:
	elm make src/Main.elm --output=main.js

run:
	npx http-server .
	