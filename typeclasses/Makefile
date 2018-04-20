slides = typeclasses

$(slides).html: $(slides).lhs
	pandoc -s --webtex -t slidy $(slides).lhs -o $(slides).html

$(slides)-nonet.html: $(slides).lhs
	pandoc -s --webtex -t slidy -V slidy-url:http://localhost:8000/ $(slides).lhs -o $(slides)-nonet.html 

.PHONY: clean runserver
clean:
	rm -f $(slides).html $(slides)-nonet.html

runserver:
	python -m http.server

open: $(slides)-nonet.html
	open http://localhost:8000/$(slides)-nonet.html
