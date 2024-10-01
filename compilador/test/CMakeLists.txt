cmake_minimum_required(VERSION 3.5)

set(PROJECT_ROOT ${CMAKE_SOURCE_DIR})
set(SOURCE_ROOT "${PROJECT_ROOT}/src")

# set(HM_TEST_SOURCES
# 	${SOURCE_ROOT}/hash_map.c
# 	hm_test.c
# )

set(LEXER_TEST_SOURCES
	${SOURCE_ROOT}/lexer.c
	lexer_test.c
)

# add_executable(hm_test ${HM_TEST_SOURCES})
add_executable(lexer_test ${LEXER_TEST_SOURCES})

include_directories(${PROJECT_ROOT}/include)

target_link_libraries(lexer_test criterion)

# Ensuring the LSP sees the includes without complaining
set(CMAKE_EXPORT_COMPILER_COMMANDS ON)

