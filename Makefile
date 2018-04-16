all: index.html

index.html: src/Main.elm src/Syntax.elm src/NNF.elm src/OmitNegations.elm src/PropositionParser.elm src/Styles.elm src/Util.elm src/Solve.elm
	elm-make $^

clean:
	rm -f index.html
