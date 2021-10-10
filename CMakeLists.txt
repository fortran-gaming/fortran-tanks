cmake_minimum_required(VERSION 3.14...3.21)

project(TankGame LANGUAGES Fortran)

add_library(utils OBJECT
src/game.f90 src/utils.f90
$<IF:$<BOOL:${WIN32}>,src/sleep_win.f90,src/sleep_unix.f90>
)

add_executable(tanks app/tanks.f90)
target_link_libraries(tanks PRIVATE utils)
