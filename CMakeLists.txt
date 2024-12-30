cmake_minimum_required(VERSION 3.26)
project(swrgm)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_ENABLE_EXPORTS On)  # turn this on so that dlsym can look up functions by name to link them

# download CPM.cmake

file(
    DOWNLOAD
        https://github.com/cpm-cmake/CPM.cmake/releases/download/v0.38.3/CPM.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/cmake/CPM.cmake
        EXPECTED_HASH SHA256=cc155ce02e7945e7b8967ddfaff0b050e958a723ef7aad3766d368940cb15494
)
include(${CMAKE_CURRENT_BINARY_DIR}/cmake/CPM.cmake)

CPMAddPackage(
    NAME Boost
    VERSION 1.84.0
    URL https://github.com/boostorg/boost/releases/download/boost-1.84.0/boost-1.84.0.tar.xz
    URL_HASH SHA256=2e64e5d79a738d0fa6fb546c6e5c2bd28f88d268a2a080546f74e5ff98f29d0e
    OPTIONS "BOOST_ENABLE_CMAKE ON"
)
CPMAddPackage("gh:fmtlib/fmt#11.1.0")

# Add zlib using CPM
CPMAddPackage(
    NAME zlib-cmake
    URL https://github.com/jimmy-park/zlib-cmake/archive/main.tar.gz
)

add_subdirectory(deps/ksd)
add_subdirectory(src)
