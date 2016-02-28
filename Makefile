README.html: README.md
	markdown README.md > README.html

.PHONY: clean
clean:
	-rm README.html

