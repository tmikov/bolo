if (EMSCRIPTEN)
  add_definitions(-DSOKOL_GLES2)
  set(SOKOL_SRC sokol.c)
  set(CMAKE_EXECUTABLE_SUFFIX ".html")
else ()
  add_definitions(-DSOKOL_GLCORE33)
#  add_definitions(-DSOKOL_METAL)
  set(SOKOL_SRC sokol.m)
endif ()

#add_compile_options(-fsanitize=address,undefined)
#add_link_options(-fsanitize=address,undefined)

set(SOURCE_FILES
  bolo.c
  ${SOKOL_SRC} sokol_app.h sokol_gfx.h sokol_glue.h)

add_executable(bolo ${SOURCE_FILES})

if (EMSCRIPTEN)
  target_link_options(bolo PRIVATE --shell-file ${CMAKE_CURRENT_SOURCE_DIR}/shell.html)
else()
  target_link_libraries(bolo
    "-framework Cocoa"
    "-framework QuartzCore"
    "-framework OpenGL")
#  target_link_libraries(bolo
#    "-framework Cocoa"
#    "-framework QuartzCore"
#    "-framework Metal"
#    "-framework MetalKit")
endif ()
