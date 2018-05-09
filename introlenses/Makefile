slides = introlenses

$(slides).html $(slides)-nonet.html: $(slides).lhs
	pandoc -s --mathjax -f markdown+inline_code_attributes+lhs -t slidy $(slides).lhs -o $(slides).html
	pandoc -s --mathjax -t slidy -V slidy-url:http://localhost:8000/ $(slides).lhs -o $(slides)-nonet.html 

clean:
	rm -f $(slides).html $(slides)-nonet.html

open: $(slides).html
	open $(slides).html

runserver:
	python -m http.server

open-nonet: $(slides)-nonet.html
	open http://localhost:8000/$(slides)-nonet.html

.PHONY: clean open runserver open-nonet