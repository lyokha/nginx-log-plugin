BASENAME := ngx_log
NAME := $(BASENAME)
PKGNAME := $(subst _,-,$(NAME))
PLUGNAME := $(BASENAME)_plugin
PKGVER := 0.1.0.0

PREFIX := /var/lib/nginx
MACHINE := $(shell uname -m)
KERNEL := $(shell uname -s | tr A-Z a-z)

PKGDISTR := ngx-export-distribution

SRC := $(NAME).hs
LIB := $(NAME).so
STUB := $(NAME)_stub.h
DISTR := $(PKGNAME)-$(PKGVER).tar.gz

GHCVER := $(shell ghc --numeric-version)
GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)
DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)
DISTDIR := dist
DISTV2DIR := dist-newstyle

INPLACE := $(shell grep -q '^packages:.*\.\./ngx-export-log' \
                       cabal.project && echo 1 || echo 0)

all: $(DISTR)

$(DISTR): $(SRC)
	cabal install --lib --only-dependencies --package-env .
ifeq ($(INPLACE),1)
	cabal build ngx-export-log
	echo "package-db $$(pwd)/$(DISTV2DIR)/packagedb/ghc-$(GHCVER)" >> \
	  $(GHCENV)
endif
	sed -i 's/\(^package-id \)/--\1/' $(GHCENV)
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs configure \
	  --package-db=clear --package-db=global \
	  $$(sed -n 's/^\(package-db\)\s\+/--\1=/p' $(GHCENV)) \
	  --prefix=$(PREFIX)
	nhm-tool deps $(PKGNAME) >> $(GHCENV)
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs build \
	  --ghc-options="$(SRC) -o $(LIB) -L$(PREFIX) -l$(PLUGNAME) $(LINKRTS)"

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner
	install $(PREFIX)/lib$(PLUGNAME).so $(PREFIX)/$(DEPLIBS)

.PHONY: clean

clean:
	rm -rf $(DISTDIR) $(DISTV2DIR) $(DEPLIBS)
	rm -f $(GHCENV) $(STUB) $(NAME).hi $(NAME).o
	rm -f $(LIB)

clean-all: clean
	rm -f $(DISTR)

