cmake_minimum_required(VERSION 2.8)
project(owlisp)



set(LLVMAPILOW ${CMAKE_CURRENT_SOURCE_DIR}/llvm_api_low.lisp)
set(SWIGFILE ${CMAKE_CURRENT_SOURCE_DIR}/swig.i)



# target for generation of llvm_api_low.lisp:
add_custom_command(
	OUTPUT ${LLVMAPILOW}
	COMMAND ${SWIG}
	ARGS -I${LLVM_INCLUDE_DIR} -cffi ${SWIGFILE}
)

add_custom_target(
	swig_generation ALL
	DEPENDS ${LLVMAPILOW}
)

