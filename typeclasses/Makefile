typeclasses.html: typeclasses.lhs
	pandoc -s --webtex -t slidy typeclasses.lhs -o typeclasses.html

typeclasses-nonet.html: typeclasses.lhs
	pandoc -s --webtex -t slidy -V slidy-url:http://localhost:8000/ typeclasses.lhs -o typeclasses-nonet.html 

.PHONY: clean runserver
clean:
	rm -f typeclasses.html typeckasses-nonet.html

runserver:
	python -m http.server

open:
	open http://localhost:8000/typeckasses-nonet.html
