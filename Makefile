.POSIX:

DESTDIR =
PREFIX = /usr
BINDIR = /bin
MANDIR = /share/man
INSTALL = install
CFLAGS = -Wall -O2
CC = gcc

# owl needs just a single binary
all owl: bin/ol

simple-ol: bin/vm
	bin/vm fasl/init.fasl --run owl/ol.scm -s none -o c/ol.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o bin/ol c/ol.c

## fasl (plain bytecode) image boostrap

fasl/boot.fasl: fasl/init.fasl
	# start bootstrapping with the bundled init.fasl image
	cp $? $@

fasl/ol.fasl: bin/vm fasl/boot.fasl owl/*.scm scheme/*.scm tests/*.scm tests/*.sh owl/*/*.scm
	# selfcompile boot.fasl until a fixed point is reached
	@bin/vm fasl/init.fasl -e '(time-ms)' >.start
	bin/vm fasl/boot.fasl --run owl/ol.scm -s none -o fasl/bootp.fasl
	@bin/vm fasl/init.fasl -e '(str"bootstrap: "(-(time-ms)(read(open-input-file".start")))"ms\nfasl: "(file-size"fasl/bootp.fasl")"b")'
	# check that the new image passes tests
	CC='$(CC)' sh tests/run all bin/vm fasl/bootp.fasl
	# copy new image to ol.fasl if it is a fixed point, otherwise recompile
	if cmp -s fasl/boot.fasl fasl/bootp.fasl; then mv fasl/bootp.fasl $@; else mv fasl/bootp.fasl fasl/boot.fasl && exec make $@; fi

## building just the virtual machine to run fasl images

bin/vm: c/vm.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $?

c/_vm.c: c/ovm.c
	# remove comments and most white-space
	sed -f bin/compact.sed $? >$@

c/vm.c: c/_vm.c
	# make a vm without a bundled heap
	echo 'static void *heap = 0;' | cat - $? >$@

manual.md: doc/manual.md owl/*.scm scheme/*.scm
	bin/find-documentation.sh | cat doc/manual.md - >$@

manual.man: manual.md
	pandoc $? -s -t man > $@

manual.pdf: manual.md
	pandoc --latex-engine xelatex -o $@ $?

## building standalone image out of the fixed point fasl image

c/ol.c: fasl/ol.fasl
	# compile the repl using the fixed point image
	bin/vm fasl/ol.fasl --run owl/ol.scm -s some -o $@

bin/ol: c/ol.c
	# compile the real owl repl binary
	$(CC) $(CFLAGS) $(LDFLAGS) -o bin/olp $?
	CC='$(CC)' CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' sh tests/run all bin/olp
	test '!' -f $@ || mv $@ bin/ol-old
	mv bin/olp $@


## running unit tests manually

fasltest: bin/vm fasl/ol.fasl
	CC='$(CC)' sh tests/run all bin/vm fasl/ol.fasl

test: bin/ol
	CC='$(CC)' sh tests/run all bin/ol

random-test: bin/vm bin/ol fasl/ol.fasl
	CC='$(CC)' sh tests/run random bin/vm fasl/ol.fasl
	CC='$(CC)' sh tests/run random bin/ol


## data

owl/unicode-char-folds.scm:
	echo "(define char-folds '(" >owl/unicode-char-folds.scm
	curl http://www.unicode.org/Public/6.0.0/ucd/CaseFolding.txt | grep "[0-9A-F]* [SFC]; " | sed -re 's/ #.*//' -e 's/( [SFC])?;//g' -e 's/^/ /' -e 's/ / #x/g' -e 's/ /(/' -e 's/$$/)/' | tr "[A-F]" "[a-f]" >> owl/unicode-char-folds.scm
	echo '))' >>owl/unicode-char-folds.scm

## meta

doc/ol.1.gz: doc/ol.1
	gzip -9n <$? >$@

doc/ovm.1.gz: doc/ovm.1
	gzip -9n <$? >$@

install: bin/ol bin/vm doc/ol.1.gz doc/ovm.1.gz
	-mkdir -p $(DESTDIR)$(PREFIX)$(BINDIR)
	-mkdir -p $(DESTDIR)$(PREFIX)$(MANDIR)/man1
	$(INSTALL) -m 755 bin/ol $(DESTDIR)$(PREFIX)$(BINDIR)/ol
	$(INSTALL) -m 755 bin/vm $(DESTDIR)$(PREFIX)$(BINDIR)/ovm
	$(INSTALL) -m 644 doc/ol.1.gz $(DESTDIR)$(PREFIX)$(MANDIR)/man1/ol.1.gz
	$(INSTALL) -m 644 doc/ovm.1.gz $(DESTDIR)$(PREFIX)$(MANDIR)/man1/ovm.1.gz

uninstall:
	-rm -f $(DESTDIR)$(PREFIX)$(BINDIR)/ol
	-rm -f $(DESTDIR)$(PREFIX)$(BINDIR)/ovm
	-rm -f $(DESTDIR)$(PREFIX)$(MANDIR)/man1/ol.1.gz
	-rm -f $(DESTDIR)$(PREFIX)$(MANDIR)/man1/ovm.1.gz

clean:
	-rm -f fasl/boot.fasl fasl/bootp.fasl fasl/ol.fasl
	-rm -f c/_vm.c c/vm.c c/ol.c
	-rm -f doc/*.gz manual.md
	-rm -f tmp/* .start
	-rm -f bin/ol bin/ol-old bin/vm

fasl-update: fasl/ol.fasl
	cp fasl/ol.fasl fasl/init.fasl

.PHONY: all clean fasl-update fasltest install owl random-test simple-ol test uninstall
