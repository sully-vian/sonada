GPRBUILD = gprbuild
GPRCLEAN = gprclean
GNATFORMAT = gnatformat 

GPRFILE = sonada.gpr
BINARY = bin/sonada

# Default target architecture
TARGET ?= native

ifeq ($(TARGET),native)
	TARGET_FLAGS =
else
	TARGET_FLAGS = --target=$(TARGET)
endif

all: release

format:
	$(GNATFORMAT) -P $(GPRFILE)

release:
	$(GPRBUILD) -p -P $(GPRFILE) $(TARGET_FLAGS) -XBUILD=release
	@if [ -f $(BINARY) ]; then strip $(BINARY); fi

debug:
	$(GPRBUILD) -p -P $(GPRFILE) $(TARGET_FLAGS) -XBUILD=debug

clean:
	$(GPRCLEAN) -P $(GPRFILE)
	rm -rf bin obj

# Example cross-compilation targets
linux-x86_64:
	$(MAKE) all TARGET=x86_64-linux-gnu

windows-x86_64:
	$(MAKE) all TARGET=x86_64-w64-mingw32

.PHONY: all debug clean linux-x86_64 windows-x86_64
