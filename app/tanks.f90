program tank_game

use game
use utils, only : rand

use, intrinsic :: iso_fortran_env, only : stdin=>input_unit

implicit none (type, external)

CHARACTER :: menu_command
integer :: i, j

call random_init(.false., .false.)

ALLOCATE (grid(nrows,ncolumns))

call start_game()

call generate_grid()

print '(/,/,/,/,/,/,/)'
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO

n_shots = 50
angle = 70
velocity = 70
wind_speed = rand()*24
wind_direction = rand()*2.0
win = 0
bomb = 0

main : DO
IF (n_shots == 0) exit main
IF (win /= 0) exit main

CALL game_display()

print '(/,A,/)', "Select an Option:   (A)ngle   |   (V)elocity   |   (F)ire"
READ(stdin,"(A)", iostat=i) menu_command
if(is_iostat_end(i)) stop "thanks for playing"
if(i/=0) cycle main

select case (menu_command)
case ("A","a")
  print '(/,/,A,I3,/,A,/)',"Your CURRENT Angle: ", angle, "Enter Angle   |   (0 > angle > 180)"
  READ(stdin, "(I3)", iostat=i) angle
  if(i/=0) cycle main
  IF (angle < 0) angle = 0
  if (angle > 180) angle = 180
case ("V","v")
  print '(/,/,A,I3,/,A,/)', " Your CURRENT Velocity: ", velocity, "Enter Velocity   |   (20 <= velocity <= 100)"
  READ (stdin, "(I3)", iostat=i) velocity
  if(i/=0) cycle main
  IF (velocity < 20) velocity = 20
  if (velocity > 100) velocity = 100
case ("B","b")
  IF (bomb == 0) THEN
    bomb = 1
  ELSE
    bomb = 0
  END IF
case ("F","f")
  CALL projectile_simulation()
  n_shots = n_shots - 1
  wind_speed = rand()*23
  wind_direction = NINT(rand())
  bomb = 0
case ("q")
  stop "thanks for playing"
end select

END DO main

print '(/,/,/,/,/,/,/)'
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO

select case (win)
case (0)
  WRITE (*,*) "Maybe Next Time ", TRIM(player_name), "..."
  score = 0
case(1)
  WRITE (*,*) "You Won ", TRIM(player_name), "!"
  print '(/,A,I3,/)', " Final Score: ", score
case default
  print '(A,/,A)', "Way to go man! :)", "You blew up your Tank!"
  score = 0
END select

END PROGRAM
