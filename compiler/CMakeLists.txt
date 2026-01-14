cmake_minimum_required(VERSION 3.28)
project(compilers_compiler CXX)

set(CMAKE_CXX_STANDARD 17)

# creates compile_commands.json
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(SOURCES
  src/main.cpp
)

add_library(compilers_compiler_lib ${SOURCES})
add_executable(compilers_compiler src/main.cpp)

target_include_directories(compilers_compiler       PRIVATE include)
target_include_directories(compilers_compiler_lib   PRIVATE include)
target_link_libraries(compilers_compiler compilers_compiler_lib -static-libgcc -static-libstdc++)
