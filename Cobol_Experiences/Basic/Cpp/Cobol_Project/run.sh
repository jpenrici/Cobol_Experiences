#!/bin/bash

# --- Path Configuration ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR/.."
BUILD_DIR="$PROJECT_ROOT/build"
LIB_DIR="$PROJECT_ROOT/lib"

# Files
LIB_SOURCE="payroll.cbl"
LIB_OBJ="$LIB_DIR/payroll.o"
MAIN_SOURCE="main.cbl"
EXE_PATH="$BUILD_DIR/test_cobol"


if [[ ! -d "$BUILD_DIR" ]]; then
    mkdir -p "$BUILD_DIR"
fi

if [[ ! -d "$LIB_DIR" ]]; then
    mkdir -p "$LIB_DIR"
fi

echo "Current Project Root: $PROJECT_ROOT"

echo "Compiling COBOL Library (Object)..."
if cobc -c -free -Wall -O "$LIB_SOURCE" -o "$LIB_OBJ"; then
    echo "SUCCESS: Object stored in $LIB_OBJ"
else
    echo "FAILED: Compilation error in $LIB_SOURCE"
    exit 1
fi

echo "Linking Main Program..."
if cobc -x -free -Wall -O "$MAIN_SOURCE" "$LIB_OBJ" -o "$EXE_PATH"; then
    echo "SUCCESS: Binary created at $EXE_PATH"
else
    echo "FAILED: Linking error with $MAIN_SOURCE"
    exit 1
fi

echo "Run..."
if [[ -f "$EXE_PATH" ]]; then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.

    "$EXE_PATH"
    EXIT_CODE=$?
    echo "-----------------------------------------------"
    echo "Execution finished (Return Code: $EXIT_CODE)"
else
    echo "ERROR: Binary not found at $EXE_PATH"
    exit 1
fi
