cmake_minimum_required(VERSION 2.8)
project(owlisp)



## locate swig:
#find_program(SWIG swig)
#if (NOT SWIG)
#	message(FATAL_ERROR "swig not found (maybe you want to tell me where I can find it by passing '-DCMAKE_PROGRAM_PATH=/path')!")
#endif (NOT SWIG)
#
#
#
## find llvm headers:
#find_path(LLVM_INCLUDE_DIR llvm-c/Core.h)
#if (NOT LLVM_INCLUDE_DIR)
#	message(FATAL_ERROR "llvm headers not found (maybe you want to tell me where I can find it by passing '-DCMAKE_INCLUDE_PATH=/path')!")
#endif (NOT LLVM_INCLUDE_DIR)
#
#
#
## find llvm library:
#set(LLVM_NAMES ${LLVM_NAMES} LLVM-3.4 LLVM-3.3 LLVM-3.2 LLVM)
#find_library(LLVM_LIBRARY NAMES ${LLVM_NAMES})
#if (NOT LLVM_LIBRARY)
#	message(FATAL_ERROR "llvm library not found (maybe you want to tell me where I can find it by passing '-DCMAKE_LIBRARY_PATH=/path')!")
#endif (NOT LLVM_LIBRARY)
#
#
#
## find llvm-link:
#find_program(LLVM_LINK llvm-link)
#if (NOT LLVM_LINK)
#	message(FATAL_ERROR "llvm-link not found (maybe you want to tell me where I can find it by passing '-DCMAKE_PROGRAM_PATH=/path')!")
#endif (NOT LLVM_LINK)
#
#
#
## find llvm-as:
#find_program(LLVM_AS llvm-as)
#if (NOT LLVM_AS)
#	message(FATAL_ERROR "llvm-as not found (maybe you want to tell me where I can find it by passing '-DCMAKE_PROGRAM_PATH=/path')!")
#endif (NOT LLVM_AS)
#
#
#
## find clang:
#find_program(CLANG clang)
#if (NOT CLANG)
#	message(FATAL_ERROR "clang not found (maybe you want to tell me where I can find it by passing '-DCMAKE_PROGRAM_PATH=/path')!")
#endif (NOT CLANG)
#
#
#
## find sbcl:
#find_program(SBCL sbcl)
#if (NOT SBCL)
#	message(FATAL_ERROR "sbcl not found (maybe you want to tell me where I can find it by passing '-DCMAKE_PROGRAM_PATH=/path')!")
#endif (NOT SBCL)
#
#
#
#add_subdirectory(src/llvm/cffi)		# defines target 'swig_generation'
#add_subdirectory(src/crt)		# defines target 'runtime_generation'
#add_subdirectory(src/crt-ng)		# builds libowlisprt.a
#
#
#
## compile owlisp-frontend:
#set(OWLISPFRONTEND ${CMAKE_CURRENT_SOURCE_DIR}/owlisp-frontend)
#set(COMPILELOG compile.log)
#
#set(MAKELISPIN ${CMAKE_CURRENT_SOURCE_DIR}/make.lisp.in)
#set(MAKELISP ${CMAKE_CURRENT_SOURCE_DIR}/make.lisp)
#configure_file(
#	${MAKELISPIN}
#	${MAKELISP}
#	@ONLY
#)
#
#set(LDLIBPATH ${CMAKE_LIBRARY_PATH} ${LD_LIBRARY_PATH})
#add_custom_command(
#	OUTPUT ${OWLISPFRONTEND}
#	COMMAND LD_LIBRARY_PATH=${LDLIBPATH} ${SBCL} --load "${MAKELISP}" >${COMPILELOG} 2>&1
#	DEPENDS swig_generation runtime_generation
#)
#
#add_custom_target(
#	owlisp-frontend_generation ALL
#	DEPENDS ${OWLISPFRONTEND}
#)
#
#set(OWLISPIN ${CMAKE_CURRENT_SOURCE_DIR}/toolchain/owlisp.in)
#set(OWLISP ${CMAKE_CURRENT_SOURCE_DIR}/toolchain/owlisp)
#configure_file(
#	${OWLISPIN}
#	${OWLISP}
#	@ONLY
#)
#
#
#
## installation instructions:
#set(RUNTIMEBC ${CMAKE_CURRENT_SOURCE_DIR}/src/crt/runtime.bc)
#
#set(BININSTALLS ${OWLISPFRONTEND} ${OWLISP})
#set(LIBINSTALLS ${RUNTIMEBC})
#
#install(
#	PROGRAMS ${BININSTALLS}
#	DESTINATION bin
#)
#install(
#	FILES ${LIBINSTALLS}
#	DESTINATION lib
#)
#
#
#
## uninstall target:
#set(UNINSTALL_FILES
#	${CMAKE_INSTALL_PREFIX}/bin/owlisp-frontend
#	${CMAKE_INSTALL_PREFIX}/bin/owlisp
#	${CMAKE_INSTALL_PREFIX}/lib/runtime.bc
#)
#add_custom_target(
#	uninstall
#	COMMAND rm -rf ${UNINSTALL_FILES}
#)

find_program(EMCC emcc)
if (EMCC)
	message(STATUS "Found emscripten: " ${EMCC})
else (EMCC)
	message(WARNING "Could not find emscripten. You will not be able to compile to JavaScript.")
endif (EMCC)

# add target owlisprt for building libowlisprt:
add_subdirectory(src/crt-ng2)

# find sbcl:
find_program(SBCL sbcl)
if (NOT SBCL)
	message(FATAL_ERROR "sbcl not found (maybe you want to tell me where I can find it by passing '-DCMAKE_PROGRAM_PATH=/path')!")
endif (NOT SBCL)

# compile owlisp2c:
set(OWLISP_L2C_BIN_FILENAME owlisp-l2c)
set(OWLISP_L2C_BIN ${PROJECT_BINARY_DIR}/${OWLISP_L2C_BIN_FILENAME})
set(COMPILELOG owlisp-l2c-build.log)

set(MAKELISPIN ${CMAKE_CURRENT_SOURCE_DIR}/make.lisp.in)
set(MAKELISP ${CMAKE_CURRENT_SOURCE_DIR}/make.lisp)
configure_file(
	${MAKELISPIN}
	${MAKELISP}
	@ONLY
)

set(LDLIBPATH ${CMAKE_LIBRARY_PATH} ${LD_LIBRARY_PATH})
#add_custom_command(
#	OUTPUT ${OWLISP2CBIN}
#	COMMAND LD_LIBRARY_PATH=${LDLIBPATH} ${SBCL} --load "${MAKELISP}" >${COMPILELOG} 2>&1
#	#DEPENDS owlisprt
#)
#
#add_custom_target(
#	clean_owlisp2c
#	COMMAND rm -vf ${OWLISP2CBIN}
#)

add_custom_target(
	owlisp-l2c ALL
	#DEPENDS clean_owlisp2c ${OWLISP2CBIN}
	COMMAND LD_LIBRARY_PATH=${LDLIBPATH} ${SBCL} --load "${MAKELISP}" >${COMPILELOG} 2>&1
)

set(OWLISP_C2O_BIN_FILENAME owlisp-c2o)
set(OWLISP_C2O_BIN_IN ${CMAKE_CURRENT_SOURCE_DIR}/bin/${OWLISP_C2O_BIN_FILENAME}.in)
set(OWLISP_C2O_BIN ${PROJECT_BINARY_DIR}/${OWLISP_C2O_BIN_FILENAME})
set(OWLISP_O2X_BIN_FILENAME owlisp-o2x)
set(OWLISP_O2X_BIN_IN ${CMAKE_CURRENT_SOURCE_DIR}/bin/${OWLISP_O2X_BIN_FILENAME}.in)
set(OWLISP_O2X_BIN ${PROJECT_BINARY_DIR}/${OWLISP_O2X_BIN_FILENAME})
set(OWLISP_BIN_FILENAME owlisp)
set(OWLISP_BIN_IN ${CMAKE_CURRENT_SOURCE_DIR}/bin/${OWLISP_BIN_FILENAME}.in)
set(OWLISP_BIN ${PROJECT_BINARY_DIR}/${OWLISP_BIN_FILENAME})
if (EMCC)
	set(OWLISP_C2BC_BIN_FILENAME owlisp-c2bc)
	set(OWLISP_C2BC_BIN_IN ${CMAKE_CURRENT_SOURCE_DIR}/bin/${OWLISP_C2BC_BIN_FILENAME}.in)
	set(OWLISP_C2BC_BIN ${PROJECT_BINARY_DIR}/${OWLISP_C2BC_BIN_FILENAME})
	set(OWLISP_BC2JS_BIN_FILENAME owlisp-bc2js)
	set(OWLISP_BC2JS_BIN_IN ${CMAKE_CURRENT_SOURCE_DIR}/bin/${OWLISP_BC2JS_BIN_FILENAME}.in)
	set(OWLISP_BC2JS_BIN ${PROJECT_BINARY_DIR}/${OWLISP_BC2JS_BIN_FILENAME})
endif (EMCC)

configure_file(
	${OWLISP_C2O_BIN_IN}
	${OWLISP_C2O_BIN}
	@ONLY
)
configure_file(
	${OWLISP_O2X_BIN_IN}
	${OWLISP_O2X_BIN}
	@ONLY
)
configure_file(
	${OWLISP_BIN_IN}
	${OWLISP_BIN}
	@ONLY
)
if (EMCC)
	configure_file(
		${OWLISP_C2BC_BIN_IN}
		${OWLISP_C2BC_BIN}
		@ONLY
	)
	configure_file(
		${OWLISP_BC2JS_BIN_IN}
		${OWLISP_BC2JS_BIN}
		@ONLY
	)
endif (EMCC)

install(
	PROGRAMS ${OWLISP_C2O_BIN} ${OWLISP_L2C_BIN} ${OWLISP_O2X_BIN} ${OWLISP_BIN}
	DESTINATION bin
)
if (EMCC)
	install(
		PROGRAMS ${OWLISP_C2BC_BIN} ${OWLISP_BC2JS_BIN}
		DESTINATION bin
)
endif (EMCC)

