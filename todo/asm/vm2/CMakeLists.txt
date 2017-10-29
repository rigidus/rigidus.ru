project ( oldgamevm C ASM-ATT )

cmake_minimum_required ( VERSION 3.5 )

# dirs
set     ( SRC_DIR "src" )
message ( STATUS "SRC_DIR: ${SRC_DIR}" )
set     ( INC_DIR "inc" )
message ( STATUS "INC_DIR: ${INC_DIR}" )

# sdl2
find_package( PkgConfig REQUIRED )
find_package( SDL2 REQUIRED )

# includes
include_directories ( ${PROJECT_NAME} ${SDL2_INCLUDE_DIRS} ${INC_DIR} )

# arch
execute_process ( COMMAND uname -m COMMAND tr -d '\n' OUTPUT_VARIABLE ARCHITECTURE )
message ( STATUS "Architecture: ${ARCHITECTURE}" )
message ( STATUS "System processor: ${CMAKE_SYSTEM_PROCESSOR}" )

# asm
set ( ASM_OK FALSE )
if ( "${CMAKE_SYSTEM_PROCESSOR}" MATCHES "x86_64" )
  enable_language ( ASM-ATT )
  if ( CMAKE_ASM-ATT_COMPILER_WORKS )
    set ( ASM_OK TRUE )
    set ( ASMS ${SRC_DIR}/asm.s )
  else ( CMAKE_ASM-ATT_COMPILER_WORKS )
    message ( ERROR ": CMAKE : ASSEMBLER AT&T COMPILER NOT WORKS !"  )
  endif ( CMAKE_ASM-ATT_COMPILER_WORKS )
else ( "${CMAKE_SYSTEM_PROCESSOR}" MATCHES "x86_64" )
  message ( ERROR ": CMAKE : SYSTEM PROCESSOR IS NOT x86_64 !"  )
endif ( "${CMAKE_SYSTEM_PROCESSOR}" MATCHES "x86_64" )

# no assembler found
if ( NOT ASM_OK )
  # TODO: file for anther arch like: set ( mySrcs ${mySrcs} codec_generic.c )
endif ( NOT ASM_OK )

# sources
set ( CSRC ${SRC_DIR}/asm.s ${SRC_DIR}/main.c ${SRC_DIR}/sdlwrap.c )
message (  STATUS "CSRC: ${CSRC}"  )

# executables
add_executable ( ${PROJECT_NAME} ${CSRC} )

# libs
target_link_libraries(oldgamevm ${SDL2_LIBRARIES})

# copy assets
file(COPY ${CMAKE_SOURCE_DIR}/assets DESTINATION  ${CMAKE_BINARY_DIR})
