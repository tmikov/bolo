add_executable(test_link test_link.c)
target_link_libraries(test_link bologame)
add_test(NAME link COMMAND test_link)

add_library(bolo_ppm STATIC ppm.c)
target_link_libraries(bolo_ppm bologame ega_render)

add_executable(test_ppm test_ppm.c)
target_link_libraries(test_ppm bolo_ppm)
add_test(NAME ppm COMMAND test_ppm)

add_executable(bolotest bolotest.c)
target_link_libraries(bolotest bolo_ppm)

add_executable(test_ega_render test_ega_render.c)
target_link_libraries(test_ega_render ega_render)
add_test(NAME ega_render COMMAND test_ega_render)

# Unlike src/bolo.c, which is deliberately faithful to 8086 assembly and
# triggers pre-existing -Wextra warnings out of scope here, emu/ code is
# ordinary C with no such excuse.
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
  foreach(target test_link bolo_ppm test_ppm bolotest test_ega_render)
    target_compile_options(${target} PRIVATE -Wall -Wextra)
  endforeach()
endif()

add_test(NAME headless_frames
         COMMAND bolotest --frames 20 --out ${CMAKE_CURRENT_BINARY_DIR}/frames-smoke)

add_test(NAME determinism
         COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/compare_runs.sh
                 $<TARGET_FILE:bolotest> ${CMAKE_CURRENT_BINARY_DIR}/det 4 4 30)

# Gameplay is gated on the timer tick, so the number of async_start() calls per
# tick must not change a single pixel.
add_test(NAME pump_invariance
         COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/compare_runs.sh
                 $<TARGET_FILE:bolotest> ${CMAKE_CURRENT_BINARY_DIR}/pump 1 8 30)
