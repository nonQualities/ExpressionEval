.PHONY: all build clean run install gen

all: build

build:
	cabal build

run:
	cabal run

install:
	cabal install :ExpressionEval
clean:
	cabal clean

gen:
	cd app/Frontend && alex Lexer.x
	cd app/Frontend && happy --ghc Parser.y
	cabal build

