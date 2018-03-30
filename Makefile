all: index.html

index.html: src/Main.elm src/Syntax.elm src/NNF.elm
	elm-make $^

clean:
	rm -f index.html
