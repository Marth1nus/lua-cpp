cmake_minimum_required(VERSION 3.29)
project(lua-cpp VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
  lua-cmake
  GIT_REPOSITORY https://github.com/Marth1nus/lua-cmake
  GIT_TAG        master
)
FetchContent_MakeAvailable(lua-cmake)

add_library(${PROJECT_NAME} src/lua-cpp.cpp)
target_link_libraries(${PROJECT_NAME} PUBLIC lua)
target_include_directories(${PROJECT_NAME} PUBLIC src)
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_23)