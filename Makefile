all: compile

compile: *.elm
	elm-make Multiplication.elm --output build/elm.js
