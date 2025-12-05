GNATFORMAT = gnatformat 

GPRFILE = sonada.gpr
BINARY = bin/sonada

# Alire setup variables
ALR_VERSION = 2.1.0
ALR_ARCHIVE = alr-$(ALR_VERSION)-bin-x86_64-linux.zip
ALR_URL = https://github.com/alire-project/alire/releases/download/v$(ALR_VERSION)/$(ALR_ARCHIVE)
TEMP_DIR = temp
ALR_ARCHIVE_PATH = $(TEMP_DIR)/$(ALR_ARCHIVE)
ALR_BIN_PATH = $(TEMP_DIR)/bin/alr

# alr command
ALR_INSTALLED = $(shell command -v alr 2> /dev/null)
ifeq ($(ALR_INSTALLED), )
	ALR = $(ALR_BIN_PATH) # set to path where bin will be decompressed
else
	ALR = $(ALR_INSTALLED) # set to global install path
endif

all: release

$(TEMP_DIR):
	mkdir -p $(TEMP_DIR)

ensure-alire:
ifeq ($(ALR_INSTALLED),)
	@echo "Alire (alr) not found. Installing Alire v$(ALR_VERSION) in $(TEMP_DIR)..."
	curl -L $(ALR_URL) -o $(ALR_ARCHIVE_PATH)
	unzip -o -d $(TEMP_DIR) $(ALR_ARCHIVE_PATH)
	$(ALR) toolchain --select gnat_native gprbuild
else
	@echo "Alire (alr) found at $(ALR). Setup skipped."
endif

format:
	$(GNATFORMAT) -P $(GPRFILE)

act:
	act --env RUNNER_TOOL_CACHE=$$HOME/.cache/act/tool_cache --env RUNNER_TEMP=$$(mktemp -d) push

release: ensure-alire
	$(ALR) build --release
	@if [ -f $(BINARY) ]; then strip $(BINARY); fi
	@echo "✅ Static binary compiled and stripped at $(BINARY)"

dev:
	$(ALR) build --development

val:
	$(ALR) build --validation

run:
	$(ALR) run

clean:
	$(ALR) clean
	rm -rf bin obj

settings:
	$(ALR) settings

update:
	alr update

.PHONY: all ensure-alire format act release dev run clean settings update
