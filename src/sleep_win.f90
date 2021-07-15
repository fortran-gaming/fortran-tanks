submodule (utils) sleep_std
use, intrinsic :: iso_c_binding, only : c_int, c_long
implicit none (type, external)

interface

subroutine winsleep(dwMilliseconds) bind (C, name='Sleep')
!! void Sleep(DWORD dwMilliseconds)
!! https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-sleep
import c_long
integer(c_long), value, intent(in) :: dwMilliseconds
end subroutine winsleep

end interface

contains

module procedure sleep

call winsleep(int(milliseconds, c_long))

end procedure sleep

end submodule sleep_std
