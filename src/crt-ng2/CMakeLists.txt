cmake_minimum_required(VERSION 2.8)
project(owlisprt)

set(owlisprt_VERSION_MAJOR 0)
set(owlisprt_VERSION_MINOR 1)

set(EMCCFLAGS -Wno-warn-absolute-paths)

#find_program(EMCC emcc)

configure_file(
	"${PROJECT_SOURCE_DIR}/include/owlisp/config.h.in"
	"${PROJECT_SOURCE_DIR}/include/owlisp/config.h"
)

include_directories(
	"${PROJECT_SOURCE_DIR}/include"
)

file(GLOB CNAMES *.c)

add_library(owlisprt ${CNAMES})

install(
	TARGETS owlisprt
	ARCHIVE DESTINATION lib/owlisp
)

install(
	DIRECTORY include/owlisp
	DESTINATION include
	FILES_MATCHING PATTERN "*.h"
)

if (EMCC)
	foreach(CNAME ${CNAMES})
		get_filename_component(CBASENAME ${CNAME} NAME_WE)
		set(BCNAME ${PROJECT_BINARY_DIR}/${CBASENAME}.bc)
		set(BCNAMES ${BCNAMES} ${BCNAME})
		add_custom_command(
			OUTPUT ${BCNAME}
			COMMAND ${EMCC} ${EMCCFLAGS} -I${CMAKE_CURRENT_SOURCE_DIR}/include -o ${BCNAME} ${CNAME}
		)
	endforeach(CNAME)
	add_custom_target(
		libowlisprt_bc ALL
		DEPENDS ${BCNAMES}
	)

	install(
		FILES ${BCNAMES}
		DESTINATION lib/owlisp
	)
endif (EMCC)

