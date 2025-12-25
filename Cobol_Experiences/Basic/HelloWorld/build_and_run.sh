#!/bin/bash

# ==============================================================================
# COBOL Build and Run Script
# Usage: ./build_run.sh [filename.cob]
# ==============================================================================

# Configuration
SOURCE_FILE="${1:-helloWorld.cob}"
BUILD_DIR="./build"

# Extract filename without extension for the output binary
EXE_NAME=$(basename "$SOURCE_FILE" .cob)
EXE_PATH="$BUILD_DIR/$EXE_NAME"

# --- 1. Dependency Check ---
# Verify if the GnuCOBOL compiler (cobc) is installed in the system
if ! command -v cobc &> /dev/null; then
    echo "Error: GnuCOBOL compiler 'cobc' not found."
    echo "Please install it using: sudo apt install gnucobol"
    exit 1
fi

# --- 2. Environment Setup ---
if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Creating build directory: $BUILD_DIR"
    mkdir -p "$BUILD_DIR"
fi

# --- 3. Compilation Phase ---
echo "Compiling: $SOURCE_FILE ..."

# cobc flags:
# -x    : Create an executable (not just a module)
# -free : Use free-form syntax (no strict column requirements)
# -o    : Specify the output path
if cobc -x -free "$SOURCE_FILE" -o "$EXE_PATH"; then
    echo "Compilation successful: $EXE_PATH"
else
    echo "Compilation failed! Please check your COBOL syntax."
    exit 1
fi

# --- 4. Execution Phase ---
# Check if the binary was actually created before running
if [[ -f "$EXE_PATH" ]]; then
    echo "--------------------------------------------------"
    echo "Running Program: $EXE_NAME"
    echo "--------------------------------------------------"

    # Execute the program
    "$EXE_PATH"

    echo "--------------------------------------------------"
    PROGRAM_EXIT_STATUS=$?
    echo "Program finished with exit code $PROGRAM_EXIT_STATUS"
else
    echo "Error: Executable not found in $EXE_PATH"
    exit 1
fi

exit 0
