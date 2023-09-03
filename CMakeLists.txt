cmake_minimum_required(VERSION 3.26)
project(swrgm)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_ENABLE_EXPORTS On)  # turn this on so that dlsym can look up functions by name to link them

find_package(ZLIB REQUIRED)

add_subdirectory(src)
