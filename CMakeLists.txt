cmake_minimum_required(VERSION 2.8)
project(owlisp)



# locate swig:
find_program(SWIG swig)
if (NOT SWIG)
	message(FATAL_ERROR "swig not found!")
endif (NOT SWIG)



# find llvm headers:
find_path(LLVM_INCLUDE_DIR llvm-c/Core.h)
if (NOT LLVM_INCLUDE_DIR)
	message(FATAL_ERROR "llvm headers not found!")
endif (NOT LLVM_INCLUDE_DIR)



# find llvm library:
set(LLVM_NAMES ${LLVM_NAMES} LLVM-3.4 LLVM-3.3 LLVM-3.2 LLVM)
find_library(LLVM_LIBRARY NAMES ${LLVM_NAMES})
if (NOT LLVM_LIBRARY)
	message(FATAL_ERROR "llvm library not found!")
endif (NOT LLVM_LIBRARY)



# find llvm-link:
find_program(LLVM_LINK llvm-link)
if (NOT LLVM_LINK)
	message(FATAL_ERROR "llvm-link not found!")
endif (NOT LLVM_LINK)



# find llvm-as:
find_program(LLVM_AS llvm-as)
if (NOT LLVM_AS)
	message(FATAL_ERROR "llvm-as not found!")
endif (NOT LLVM_AS)



# find clang:
find_program(CLANG clang)
if (NOT CLANG)
	message(FATAL_ERROR "clang not found!")
endif (NOT CLANG)



# find sbcl:
find_program(SBCL sbcl)
if (NOT SBCL)
	message(FATAL_ERROR "sbcl not found!")
endif (NOT SBCL)



add_subdirectory(src/llvm/cffi)		# defines target 'swig_generation'
add_subdirectory(src/llvm/crt)		# defines target 'runtime_generation'



set(OWLISPFRONTEND ${CMAKE_CURRENT_SOURCE_DIR}/owlisp-frontend)

set(MAKELISPIN ${CMAKE_CURRENT_SOURCE_DIR}/make.lisp.in)
set(MAKELISP ${CMAKE_CURRENT_SOURCE_DIR}/make.lisp)
configure_file(
	${MAKELISPIN}
	${MAKELISP}
	@ONLY
)

add_custom_command(
	OUTPUT ${OWLISPFRONTEND}
	COMMAND ${SBCL} --load "${MAKELISP}"
	DEPENDS swig_generation runtime_generation
)

add_custom_target(
	owlisp-frontend_generation ALL
	DEPENDS ${OWLISPFRONTEND}
)

