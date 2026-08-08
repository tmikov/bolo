add_executable(test_link test_link.c)
target_link_libraries(test_link bologame)
add_test(NAME link COMMAND test_link)
