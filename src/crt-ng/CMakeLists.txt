cmake_minimum_required(VERSION 2.8)
project(owlisp)



# find all .c files:
file(GLOB CNAMES *.c)
#set(CNAMES
#	${CMAKE_CURRENT_SOURCE_DIR}/binding.c
#	${CMAKE_CURRENT_SOURCE_DIR}/environment.c
#	${CMAKE_CURRENT_SOURCE_DIR}/procedure.c
#	${CMAKE_CURRENT_SOURCE_DIR}/value.c
#)

add_library(owlisprt ${CNAMES})
