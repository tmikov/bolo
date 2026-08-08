if (EMSCRIPTEN)
  add_definitions(-DSOKOL_GLES2)
  set(SOKOL_SRC sokol.c)
  set(CMAKE_EXECUTABLE_SUFFIX ".html")
elseif (APPLE)
  add_definitions(-DSOKOL_GLCORE33)
#  add_definitions(-DSOKOL_METAL)
  # sokol_app/sokol_audio use Objective-C on Apple platforms.
  set(SOKOL_SRC sokol.m)
else ()
  # Linux and other X11 platforms. sokol_app only supports GL there.
  add_definitions(-DSOKOL_GLCORE33)
  set(SOKOL_SRC sokol.c)
endif ()

#add_compile_options(-fsanitize=address,undefined)
#add_link_options(-fsanitize=address,undefined)

set(SOURCE_FILES
  bolo.c
  ${SOKOL_SRC} sokol_app.h sokol_gfx.h sokol_glue.h)

add_executable(bolo ${SOURCE_FILES})

if (EMSCRIPTEN)
  target_link_options(bolo PRIVATE --shell-file ${CMAKE_CURRENT_SOURCE_DIR}/shell.html)
elseif (APPLE)
  target_link_libraries(bolo
    "-framework Cocoa"
    "-framework QuartzCore"
    "-framework OpenGL"
    "-framework AudioToolbox")
#  target_link_libraries(bolo
#    "-framework Cocoa"
#    "-framework QuartzCore"
#    "-framework Metal"
#    "-framework MetalKit")
else ()
  # sokol_app.h wants X11, Xi, Xcursor, GL, dl, pthread, m; sokol_audio.h wants
  # ALSA. It also requires -pthread, which Threads::Threads supplies.
  find_package(X11 REQUIRED)
  find_package(OpenGL REQUIRED)
  find_package(ALSA REQUIRED)
  set(THREADS_PREFER_PTHREAD_FLAG ON)
  find_package(Threads REQUIRED)

  if (NOT X11_Xi_FOUND OR NOT X11_Xcursor_FOUND)
    message(FATAL_ERROR "The Xi and Xcursor X11 extensions are required (libxi-dev, libxcursor-dev)")
  endif ()

  target_include_directories(bolo PRIVATE ${X11_INCLUDE_DIR} ${ALSA_INCLUDE_DIRS})
  target_link_libraries(bolo
    ${X11_LIBRARIES}
    ${X11_Xi_LIB}
    ${X11_Xcursor_LIB}
    OpenGL::GL
    ${ALSA_LIBRARIES}
    Threads::Threads
    ${CMAKE_DL_LIBS}
    m)
endif ()
