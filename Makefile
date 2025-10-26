# Makefile for fit - Terminal-based merge conflict resolver
.NOTPARALLEL:

FC := gfortran
FFLAGS := -O2 -ffree-line-length-none -J build/
LDFLAGS :=

SRC_DIR := src
APP_DIR := app
BUILD_DIR := build
BIN_DIR := bin

TARGET := $(BIN_DIR)/fit

# Source files (order matters - modules first)
MODULES := $(SRC_DIR)/terminal_control.f90 \
           $(SRC_DIR)/pane_state.f90 \
           $(SRC_DIR)/conflict_parser.f90 \
           $(SRC_DIR)/resolution_engine.f90 \
           $(SRC_DIR)/keyboard_input.f90 \
           $(SRC_DIR)/tui_layout.f90

MAIN := $(APP_DIR)/main.f90

# Object files
MOD_OBJS := $(patsubst $(SRC_DIR)/%.f90,$(BUILD_DIR)/%.o,$(MODULES))
MAIN_OBJ := $(BUILD_DIR)/main.o

.PHONY: all clean install test release

all: $(TARGET)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Build modules in order
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Build main program
$(MAIN_OBJ): $(MAIN) $(MOD_OBJS) | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Link everything
$(TARGET): $(MOD_OBJS) $(MAIN_OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^

release: FFLAGS := -O3 -ffree-line-length-none -J build/
release: clean all

test: $(TARGET)
	@echo "Running tests..."
	@if [ -d test ]; then \
		cd test && bash run_tests.sh || true; \
	else \
		echo "No tests found"; \
	fi

install: $(TARGET)
	install -Dm755 $(TARGET) $(DESTDIR)/usr/bin/fit

clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)

# For flat installation (RPM builds)
fit: $(TARGET)
	cp $(TARGET) ./fit
