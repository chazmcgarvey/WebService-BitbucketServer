
# This is not a Perl distribution, but it can build one using Dist::Zilla.

CPANM   = cpanm
DZIL    = dzil
PROVE   = prove

all: bootstrap dist

bootstrap:
	$(CPANM) Dist::Zilla
	$(DZIL) authordeps --missing |$(CPANM) -n
	$(DZIL) listdeps --develop --missing |$(CPANM) -n

clean:
	$(DZIL) $@

dist:
	$(DZIL) build

test:
	$(PROVE) -l

.PHONY: all bootstrap clean dist test

