# use installed alire or local one: ./bin/alr
ALR = $(shell command -v alr 2> /dev/null || echo bin/alr)
GNATFORMAT = gnatformat 

GPRFILE = sonada.gpr
BINARY = bin/sonada

ALR_VERSION = 2.1.0
ALR_ARCHIVE = alr-$(ALR_VERSION)-bin-x86_64-linux.zip
ALR_URL = https://github.com/alire-project/alire/releases/download/v$(ALR_VERSION)/$(ALR_ARCHIVE)

all: release

setup-alire:
	curl -L -O $(ALR_URL)
	unzip $(ALR_ARCHIVE)
	$(ALR) toolchain --select gnat_native gprbuild

format:
	$(GNATFORMAT) -P $(GPRFILE)

act:
	act push

release:
	$(ALR) build --release
	@if [ -f $(BINARY) ]; then strip $(BINARY); fi

dev:
	$(ALR) build --development

run:
	$(ALR) run

clean:
	$(ALR) clean
	rm -rf bin obj

settings:
	$(ALR) settings

update:
	alr update

.PHONY: all setup-alire format act release dev run clean settings update
