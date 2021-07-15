module utils

use, intrinsic :: iso_fortran_env, only : real64

implicit none (type, external)

interface !< sleep*.f90
module subroutine sleep(milliseconds)
integer, intent(in) :: milliseconds
end subroutine sleep
end interface

contains

impure elemental real(real64) function rand()
call random_number(rand)
end function rand

end module utils
