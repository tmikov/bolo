add_definitions(-DSOKOL_GLCORE33)

set(SOURCE_FILES
  bolo.c
  sokol.m sokol_app.h sokol_gfx.h sokol_glue.h)

add_executable(bolo ${SOURCE_FILES})

target_link_libraries(bolo
  "-framework Cocoa"
  "-framework QuartzCore"
  "-framework OpenGL"
  )
