cmake_minimum_required(VERSION 3.5)
project(Compositor)

# Compiler flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Werror -pedantic -Wno-implicit-function-declaration -std=c11")
set(CMAKE_VERBOSE_MAKEFILE ON)

# Directories
set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)
set(INCLUDE_DIR ${CMAKE_SOURCE_DIR}/include)
set(BIN_DIR ${CMAKE_SOURCE_DIR}/bin)
set(BUILD_DIR ${CMAKE_SOURCE_DIR}/build)
set(TEST_DIR ${CMAKE_SOURCE_DIR/test})

# Tests 
add_subdirectory(test)

# Executable
set(TARGET_NAME compositor)
set(TARGET ${BIN_DIR}/${TARGET_NAME})

# Source files
file(GLOB_RECURSE SRCS ${SRC_DIR}/*.c)

# Include directories
include_directories(${INCLUDE_DIR})

# Generate executable
add_executable(${TARGET_NAME} ${SRCS})

# Set output directory for executable
set_target_properties(${TARGET_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${BIN_DIR})

# Ensuring the LSP sees the includes without complaining
set(CMAKE_EXPORT_COMPILER_COMMANDS ON)
