# clatter-clim Makefile
# Builds and installs a standalone executable using SBCL.

SBCL    := sbcl
DSS     := --dynamic-space-size 4096
TARGET  := clatter-clim
BUILD   := build.lisp
PREFIX  ?= /usr/local
BINDIR  := $(PREFIX)/bin

# Register the ~/SourceCode tree so ASDF finds clatter-clim and its sibling
# checkouts (clatter-irc, McCLIM, charmed, charmed-mcclim, clim-clog).
REGISTRY := (asdf:initialize-source-registry (list :source-registry (list :tree (merge-pathnames "SourceCode/" (user-homedir-pathname))) :inherit-configuration))

.PHONY: all build check run dev install uninstall clean

all: build

# Build the compressed executable into bin/clatter-clim.
build:
	$(SBCL) $(DSS) --non-interactive --load $(BUILD)
	@echo "Done: bin/$(TARGET)"

# Compile-check only: load the system, report OK or FAIL, do not build an image.
check:
	$(SBCL) $(DSS) --non-interactive \
	  --eval '(require :asdf)' \
	  --eval '$(REGISTRY)' \
	  --eval '(handler-case (progn (asdf:load-system :clatter-clim) (format t "~&CHECK OK~%")) (error (e) (format *error-output* "~&CHECK FAIL: ~A~%" e) (uiop:quit 1)))'

# Run the built binary (native CLX backend).
run: build
	./bin/$(TARGET)

# Run from source without building (native backend).
dev:
	$(SBCL) $(DSS) \
	  --eval '(require :asdf)' \
	  --eval '$(REGISTRY)' \
	  --eval '(asdf:load-system :clatter-clim)' \
	  --eval '(clatter-clim:run :new-process nil)'

# Install to $(PREFIX)/bin (use sudo for the default /usr/local).
install: build
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 bin/$(TARGET) $(DESTDIR)$(BINDIR)/$(TARGET)
	@echo "Installed $(DESTDIR)$(BINDIR)/$(TARGET)"

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET)

clean:
	rm -rf bin/
