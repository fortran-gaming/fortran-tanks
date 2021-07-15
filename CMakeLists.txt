cmake_minimum_required(VERSION 3.14...3.21)

project(TankGame LANGUAGES Fortran)

add_library(utils OBJECT src/game.f90 src/utils.f90)
if(WIN32)
  target_sources(utils PRIVATE src/sleep_win.f90)
else()
  target_sources(utils PRIVATE src/sleep_unix.f90)
endif()

add_executable(tanks app/tanks.f90)
target_link_libraries(tanks PRIVATE utils)
