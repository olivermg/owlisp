cmake_minimum_required(VERSION 2.8)
project(owlisprt)

set(owlisprt_VERSION_MAJOR 0)
set(owlisprt_VERSION_MINOR 1)

configure_file(
	"${PROJECT_SOURCE_DIR}/include/owlisp/config.h.in"
	"${PROJECT_SOURCE_DIR}/include/owlisp/config.h"
)

include_directories(
	"${PROJECT_SOURCE_DIR}/include"
)

add_library(owlisprt
	classes.c
	env.c
	invocation.c
	math_basic.c
	print.c
)

install(
	TARGETS owlisprt
	ARCHIVE DESTINATION lib
)

install(
	DIRECTORY include/owlisp
	DESTINATION include
	FILES_MATCHING PATTERN "*.h"
)
