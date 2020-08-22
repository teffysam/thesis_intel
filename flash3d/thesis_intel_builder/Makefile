all: thesis_intel_builder

WARNINGS = -Wall
DEBUG = -ggdb -fno-omit-frame-pointer
OPTIMIZE = -O2

thesis_intel_builder: Makefile thesis_intel_builder.c
	$(CC) -o $@ $(WARNINGS) $(DEBUG) $(OPTIMIZE) thesis_intel_builder.c

clean:
	rm -f thesis_intel_builder

# Builder will call this to install the application before running.
install:
	echo "Installing is not supported"

# Builder uses this target to run your application.
run:
	./thesis_intel_builder

