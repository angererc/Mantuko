.SUFFIXES: .erl .yrl .xrl .html

LEXERS = src/frontend/lexer
PARSERS= src/frontend/parser

DOC_FILES = $(wildcard src/*/*.erl)
DOCDIR = doc

all: compile

parsers: $(LEXERS:%=%.erl) $(PARSERS:%=%.erl)
	
compile: parsers
	mkdir -p ebin
	mkdir -p test/ebin
	erl -make

.PHONY: clean
clean:
	rm -rf ./ebin
	rm -rf ./test/ebin
	rm -rf ./doc

test: compile
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-s test_suite test \
		-s init stop

#see http://www.gnu.org/software/make/manual/make.html for how to transform a list into a comma-separated list
quote:= "
comma:= ,
quotecomma:= $(quote)$(comma)$(quote)
empty:= 
space:= $(empty) $(empty)
doc: $(DOC_FILES)
	mkdir -p doc
	erl -noshell -run edoc_run files '["$(subst $(space),$(quotecomma),$(DOC_FILES))"]' '[{dir,"$(DOCDIR)"}]' 
		
.xrl.erl:
	erl -noshell -s init stop -eval 'leex:file("$<")'

.yrl.erl:
	erl -noshell -s init stop -eval 'yecc:file("$<", [{verbose, true}])'
