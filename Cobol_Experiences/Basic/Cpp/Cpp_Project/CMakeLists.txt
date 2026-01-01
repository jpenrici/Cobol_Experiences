cmake_minimum_required(VERSION 3.25 FATAL_ERROR)

project(Cpp_Project LANGUAGES CXX)

set(PROGRAM_NAME payroll_app)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# --- Path Configuration ---
set(COBOL_LIB_PATH "${CMAKE_SOURCE_DIR}/../lib/payroll.o")

# --- Validation: Check if COBOL object exists ---
if(EXISTS "${COBOL_LIB_PATH}")
  message(STATUS "Found COBOL object: ${COBOL_LIB_PATH}")
else()
  message(FATAL_ERROR "COBOL object NOT FOUND at ${COBOL_LIB_PATH}.
    Please run the COBOL shell script first to generate the library.")
endif()

# --- Find GnuCOBOL Runtime ---
find_library(COBOL_LIB NAMES cob REQUIRED)

if(NOT COBOL_LIB)
  message(
    FATAL_ERROR
      "GnuCOBOL runtime (libcob) not found. Please install libcob-dev or similar."
  )
endif()

set(SOURCES src/main.cpp)

add_executable(${PROGRAM_NAME} ${SOURCES})

# --- Linking ---
target_link_libraries(
  ${PROGRAM_NAME} PRIVATE "${CMAKE_SOURCE_DIR}/../lib/payroll.o" ${COBOL_LIB})
