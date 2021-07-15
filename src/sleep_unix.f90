submodule (utils) sleep_std
use, intrinsic :: iso_c_binding, only : c_int, c_long
implicit none (type, external)

interface

integer(c_int) function usleep(usec) bind (C)
!! int usleep(useconds_t usec);
!! https://linux.die.net/man/3/usleep
import c_int
integer(c_int), value, intent(in) :: usec
end function usleep
end interface

contains

module procedure sleep
integer(c_int) :: ierr

ierr = usleep(int(milliseconds * 1000, c_int))
if (ierr/=0) error stop 'problem with usleep() system call'

end procedure sleep

end submodule sleep_std
