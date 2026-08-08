add_executable(test_link test_link.c)
target_link_libraries(test_link bologame)
add_test(NAME link COMMAND test_link)

add_library(bolo_ppm STATIC ppm.c)
target_link_libraries(bolo_ppm bologame)

add_executable(test_ppm test_ppm.c)
target_link_libraries(test_ppm bolo_ppm)
add_test(NAME ppm COMMAND test_ppm)
