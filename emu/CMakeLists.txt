add_executable(test_link test_link.c)
target_link_libraries(test_link bologame)
add_test(NAME link COMMAND test_link)

add_library(bolo_ppm STATIC ppm.c)
target_link_libraries(bolo_ppm bologame ega_render)

add_executable(test_ppm test_ppm.c)
target_link_libraries(test_ppm bolo_ppm)
add_test(NAME ppm COMMAND test_ppm)

add_executable(test_state_table test_state_table.c)
target_link_libraries(test_state_table bologame)
add_test(NAME state_table COMMAND test_state_table)

add_executable(bolotest bolotest.c)

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

add_library(bolo_runner STATIC runner.c)
target_include_directories(bolo_runner PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(bolo_runner bolo_machine)

add_executable(test_runner test_runner.c)
target_link_libraries(test_runner bolo_runner)
add_test(NAME runner COMMAND test_runner)

# bolotest drives both sides, so it is linked here, after the runner exists.
target_link_libraries(bolotest bolo_ppm bolo_runner)

# Unlike src/bolo.c, which is deliberately faithful to 8086 assembly and
# triggers pre-existing -Wextra warnings out of scope here, emu/ code is
# ordinary C with no such excuse.
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
  foreach(target test_link bolo_ppm test_ppm bolotest test_ega_render
                 bolo_lst test_lst bolo_i8086 test_decode test_alu test_exec
                 bolo_machine test_machine bolo_runner test_runner)
    target_compile_options(${target} PRIVATE -Wall -Wextra)
  endforeach()
endif()

# Pins the interpreter's behavior independently of the C port: one line per
# frame of "index time_tick FNV-1a-of-all-four-planes" for the original's first
# 60 frames. A failure here means the interpreter, the machine or the run loop
# changed what the original does -- decide whether that was a bug or a fix
# before regenerating golden-original.txt.
add_test(NAME golden COMMAND bolotest --original ${CMAKE_CURRENT_BINARY_DIR}/golden-run
                             --frames 60
                             --golden ${CMAKE_CURRENT_BINARY_DIR}/golden-out.txt
                             --check-golden ${CMAKE_CURRENT_SOURCE_DIR}/golden-original.txt)

add_test(NAME headless_frames
         COMMAND bolotest --frames 20 --out ${CMAKE_CURRENT_BINARY_DIR}/frames-smoke)

add_test(NAME determinism
         COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/compare_runs.sh
                 $<TARGET_FILE:bolotest> ${CMAKE_CURRENT_BINARY_DIR}/det 4 4 30)

# The ratchet: run the port and the original side by side and check that the
# port still matches the original for at least as many frames as baseline.txt
# records. It fails only on a regression; an improvement is reported and left
# for a human to commit. A divergence is a finding to investigate, never
# something to tune away by adjusting the comparison.
add_test(NAME compare COMMAND bolotest --compare --ticks 800
                              --out ${CMAKE_CURRENT_BINARY_DIR}/cmp
                              --baseline ${CMAKE_CURRENT_SOURCE_DIR}/baseline.txt)

# Gameplay is gated on the timer tick, so the number of async_start() calls per
# tick must not change a single pixel.
add_test(NAME pump_invariance
         COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/compare_runs.sh
                 $<TARGET_FILE:bolotest> ${CMAKE_CURRENT_BINARY_DIR}/pump 1 8 30)
