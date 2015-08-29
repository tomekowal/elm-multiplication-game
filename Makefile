all: compile

compile: Multiplication.elm
	elm-make Multiplication.elm --output build/elm.js
