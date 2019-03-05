src=async_futures.md
result=async_futures.html

revealurl=https://revealjs.com

$(result): $(src)
	pandoc -s -f markdown -t revealjs -V revealjs-url=$(revealurl) ./$(src) -o $(result)

.PHONY: clean
clean:
	rm -f $(result)

open: $(result)
	open $(result)
