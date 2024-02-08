SUBDIRS = src
SHELL   = /bin/bash
VERS    = $(shell head -1 src/alps.c | cut -d '-' -f 2 | tr "." "-")
SAVE_FILES = .gitignore Issues.txt Makefile Todo.txt


all:
	@for I in ${SUBDIRS}; do (cd $$I; ${MAKE} $@ || exit 1); done

alps:	lclean
	@for I in ${SUBDIRS}; do (cd $$I; ${MAKE} $@ || exit 1); done

alpsf:  lclean
	@for I in ${SUBDIRS}; do (cd $$I; ${MAKE} $@ || exit 1); done

TAGS:
	@for I in ${SUBDIRS}; do (cd $$I; ${MAKE} $@ || exit 1); done

tar:    clean 
	tar czvf alps$(VERS).tgz  $(SAVE_FILES) data doc src lisp local priv

rtar:	clean
	tar czvf alpsSrc$(VERS).tgz  $(SAVE_FILES) data doc src lisp

release: alps alpsf 
	@echo "Making release " $(VERS)
	rm -rf alps$(VERS)
	mkdir alps$(VERS)
	cp -R alps alpsf data doc/userdoc.txt lisp alps$(VERS)
	cp lisp/dotalps alps$(VERS)/.alps
	tar czvf alpsR$(VERS).tgz alps$(VERS)

install: alps
	ln -f alps ~/bin/alps

.PHONY: lclean clean Changelog

clean: 
	$(RM) alps alpsf core
	@for I in ${SUBDIRS}; do (cd $$I; ${MAKE} $@ || exit 1); done

Changelog:
	git log --pretty=format:"%ai %ae%n    %h:%w(0,1,8)%B" >Changelog
