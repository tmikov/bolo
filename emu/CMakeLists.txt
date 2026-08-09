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

# The 8086 interpreter and its LST oracle. Standalone: no bologame dependency.
add_library(bolo_lst STATIC lst.c)
target_include_directories(bolo_lst PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_definitions(bolo_lst PUBLIC
    BOLO_LST_PATH="${CMAKE_SOURCE_DIR}/disasm/BOLO.LST"
    BOLO_COM_PATH="${CMAKE_SOURCE_DIR}/disasm/BOLO.COM")

add_executable(test_lst test_lst.c)
target_link_libraries(test_lst bolo_lst)
add_test(NAME lst COMMAND test_lst)

add_library(bolo_i8086 STATIC i8086.c)
target_include_directories(bolo_i8086 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(test_decode test_decode.c)
target_link_libraries(test_decode bolo_i8086 bolo_lst)
add_test(NAME decode COMMAND test_decode)

add_executable(test_alu test_alu.c)
target_link_libraries(test_alu bolo_i8086)
add_test(NAME alu COMMAND test_alu)

add_executable(test_exec test_exec.c)
target_link_libraries(test_exec bolo_i8086)
add_test(NAME exec COMMAND test_exec)

add_library(bolo_machine STATIC machine.c)
target_include_directories(bolo_machine PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(bolo_machine bolo_i8086 bologame)
target_compile_definitions(bolo_machine PUBLIC
    BOLO_COM_PATH="${CMAKE_SOURCE_DIR}/disasm/BOLO.COM")

add_executable(test_machine test_machine.c)
target_link_libraries(test_machine bolo_machine)
add_test(NAME machine COMMAND test_machine)

# Unlike src/bolo.c, which is deliberately faithful to 8086 assembly and
# triggers pre-existing -Wextra warnings out of scope here, emu/ code is
# ordinary C with no such excuse.
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
  foreach(target test_link bolo_ppm test_ppm bolotest test_ega_render
                 bolo_lst test_lst bolo_i8086 test_decode test_alu test_exec
                 bolo_machine test_machine)
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
