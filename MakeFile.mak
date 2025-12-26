all : build

build: 
	stack build

install: 
	stack install

rebuild:
	stack clean
	cd ./src/Frontend && stack exec alex Lexer.x
	cd ./src/Frontend && exec --happy --ghc Parser.y
	stack build