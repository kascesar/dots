.PHONY: readme pdf all

all: readme pdf

readme:
	python3 gen_readme.py

pdf: README.md
	pandoc README.md -o README.pdf --pdf-engine=xelatex

README.md: gen_readme.py keymap.c
	python3 gen_readme.py
