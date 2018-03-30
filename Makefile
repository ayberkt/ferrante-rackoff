all: index.html

index.html: src/Main.elm
	elm-make $<

clean:
	rm -f index.html
