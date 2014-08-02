cmake_minimum_required(VERSION 2.8)
project(owlisp)



# find all .c files:
#file(GLOB CNAMES *.c)
set(CNAMES
	${CMAKE_CURRENT_SOURCE_DIR}/gc.c
	${CMAKE_CURRENT_SOURCE_DIR}/error.c
	${CMAKE_CURRENT_SOURCE_DIR}/types.c
	${CMAKE_CURRENT_SOURCE_DIR}/frame.c
	${CMAKE_CURRENT_SOURCE_DIR}/arecord.c
)



# command for generation of .bc files from .c files:
foreach(CNAME ${CNAMES})
	get_filename_component(BASENAME ${CNAME} NAME_WE)
	set(BCNAME ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.bc)
	set(LLNAME ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.ll)
	set(BCNAMES ${BCNAMES} ${BCNAME})
	add_custom_command(
		OUTPUT ${LLNAME}
		COMMAND ${CLANG} -Wall -emit-llvm -S -o ${LLNAME} ${CNAME}
	)
	add_custom_command(
		OUTPUT ${BCNAME}
		COMMAND ${LLVM_AS} -o=${BCNAME} ${LLNAME}
		DEPENDS ${LLNAME}
	)
endforeach(CNAME)



# command & target for linking all .bc files to runtime.bc:
set(RUNTIMEBC ${CMAKE_CURRENT_SOURCE_DIR}/runtime.bc)
add_custom_command(
	OUTPUT ${RUNTIMEBC}
	COMMAND ${LLVM_LINK} -o ${RUNTIMEBC} ${BCNAMES}
	DEPENDS ${BCNAMES}
)

add_custom_target(
	runtime_generation ALL
	DEPENDS ${RUNTIMEBC}
)

