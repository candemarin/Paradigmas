# Si tienen Ubuntu se instala con "snap install typst"
TYPST = typst
FILE = demo

all: $(FILE).pdf

$(FILE).pdf: $(FILE).typ
	$(TYPST) compile $< $@

