module game

use utils, only : rand, sleep
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stderr=>error_unit

implicit none (type, external)


CHARACTER(10) :: player_name, hill_level, wind_level
character, allocatable :: grid(:,:)
REAL :: w_l

integer, parameter :: nrows = 50, ncolumns = 100
integer ::  t_c1, t_c2, hit_count, win, tank_column, tank_row, ierr
integer :: x(23), y(23)
INTEGER :: n_shots, angle, velocity, wind_speed, wind_direction, score, clock, bomb, load_space
REAL :: h, rise, h_w

contains

subroutine start_game()

CHARACTER :: response

grid = " "
grid(nrows,:) = "="
print '(/,/,/,/,/,/,/,/,/,/)'
print '(A)', "                             //"
print '(A)', "                            //"
print '(A)', "                          /\/\"
print '(A)', "                         /\/\/"
print '(A)', "                        /\/\/"
print '(A)', "            ___________/\/\/______"
print '(A)', "     ______(______________________)_____"
print '(A)', "    ||                                 ||"
print '(A)', "     \\\                               //"
print '(A)', "      \\\      WELCOME to TANK!       //"
print '(A)', "       \\\                           //"
print '(A)', "        \\\_________________________//"
print '(A)', "         (/\/\/\/\/\/\/\/\/\/\/\/\/)"

print '(/,/,/,/,/,/,A,/)', "Choose a Level for Wind:  (L)ow  |  (M)edium  |  (H)igh"

wind_level = ""

do while (wind_level=="")
  read(stdin,'(A1)') response

  select case (response)
  case ("L","l")
  w_l = 0.01
  wind_level = "Low"
  case ("M","m")
  w_l = 0.03
  wind_level = "Medium"
  case ("H","h")
  w_l = 0.07
  wind_level = "High"
  case default
  write(stderr,*) "invalid response: " // response
  END select
end do

end subroutine start_game


subroutine generate_grid()

integer :: i, j,k, l, m

h = 0.9*nrows
l = 0
m = 0
h_w = rand()*(ncolumns/10)+10.0
DO k = 1, ncolumns
load_space = ncolumns/10

301 FORMAT (A1)
302 FORMAT (A3)
IF (l == 0) THEN
DO
rise = rand()*2.0-1.0
IF (rise <= 0) THEN
m = m + 1
EXIT
END IF
END DO
END IF
IF (l == 1) THEN
DO
rise = rand()*2.0-1.0
IF (rise >= 0) THEN
m = m + 1
EXIT
END IF
END DO
END IF
IF (m >= NINT(h_w)) THEN
  IF (l == 0) THEN
    l = 1
    h_w = rand()*(ncolumns/10)+10.0
  ELSE IF (l == 1) THEN
    l = 0
    h_w = rand()*(ncolumns/10)+10.0
  END IF
  m = 0
END IF
IF (NINT(h)+NINT(rise) <= 3) rise=0
IF (NINT(h)+NINT(rise) >= nrows-3) rise=0
DO i = NINT(h)+NINT(rise),nrows-1
  grid(i,k) = "~"
END DO
h = h + rise
END DO
print '(/,/,/,/,/,/,/)'
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO
tank_column = NINT(rand()*ncolumns*0.9)+5
DO i = 1, nrows
IF (grid(i,tank_column) == "~" .OR. grid(i,tank_column) == "=") THEN
grid(i-1,tank_column) = "T"
grid(i-1,tank_column-1) = " "
grid(i-1,tank_column+1) = " "
tank_row = i-1
EXIT
END IF
END DO
k = 0
l = 0
t_c1 = 1000
t_c2 = 1000
DO
j = NINT(rand()*ncolumns*0.9)+5
IF (ABS(tank_column-j) > (ncolumns*0.15) .AND. ABS(t_c1-j) > (ncolumns*0.05) .AND. ABS(t_c2-j) > (ncolumns*0.05)) THEN
  DO i = 1, nrows
    IF (grid(i,j) == "~" .OR. grid(i,j) == "=") THEN
      grid(i-1,j) = "X"
      grid(i-1,j+1) = "X"
      grid(i-2,j) = "X"
      grid(i-2,j+1) = "X"
      EXIT
    END IF
  END DO
k = k + 1
l = 1
END IF
IF (k == 1 .AND. l == 1) THEN
  t_c1 = j
  l = 0
ELSE IF (k == 2 .AND. l == 1) THEN
  t_c2 = j
  l = 0
END IF
IF (k == 3) EXIT
END DO

end subroutine generate_grid



SUBROUTINE projectile_simulation()

REAL :: speed(100000)
REAL :: time(100000), range(100000), altitude(100000), accel_x, sim_angle, time_final, altitude_step, range_step
INTEGER :: step_final
INTEGER :: i, j, k, l, m

integer, parameter :: ptime = 10, ptime2 = 100

i = 1
time = 0
IF (wind_direction == 0) accel_x = -REAL(wind_speed)*w_l
IF (wind_direction == 1) accel_x = REAL(wind_speed)*w_l
sim_angle = REAL(angle)*3.14159/180.0
altitude_step = 510/(REAL(tank_row)*2.0)
range_step = 1260/(REAL(ncolumns)*2.0)
DO
  time(i) = REAL(i)*0.03
  range(i) = REAL(velocity)*COS(sim_angle)*time(i) + 0.5*accel_x*time(i)**2
  altitude(i) = REAL(velocity)*SIN(sim_angle)*time(i) - 0.5*9.81*time(i)**2
  speed(i) = SQRT((REAL(velocity)*COS(sim_angle)+accel_x*time(i))**2+(REAL(velocity)*SIN(sim_angle)-9.81*time(i))**2)
  IF (altitude(i) < -510) exit
  i = i + 1
END DO

step_final = i
time_final = time(i)
hit_count = 0
DO k = 1, step_final
  j = FLOOR(range(k)/range_step)
  i = FLOOR(tank_row - 1 - altitude(k)/altitude_step)
  IF (i > 0 .AND. i < (nrows-1) .AND. j+tank_column > 1 .AND. j+tank_column < ncolumns) THEN
  IF (grid(i,j+tank_column) == " ") THEN
    grid(i,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(int(0.2*speed(i)))
  ELSE IF (grid(i,j+tank_column) == "~") THEN
    grid(i,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i-1,j+tank_column) == "X") hit_count = 1
    IF (i-1 /= nrows) grid(i-1,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i-1,j+tank_column+1) == "X") hit_count = 1
    IF (i-1 < nrows) grid(i-1,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i,j+tank_column+1) == "X") hit_count = 1
    IF (i < nrows) grid(i,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i+1,j+tank_column+1) == "X") hit_count = 1
    IF (i+1 < nrows) grid(i+1,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i+1,j+tank_column) == "X") hit_count = 1
    IF (i+1 < nrows) grid(i+1,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i+1,j+tank_column-1) == "X") hit_count = 1
    IF (i+1 < nrows) grid(i+1,j+tank_column-1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i,j+tank_column-1) == "X") hit_count = 1
    IF (i < nrows) grid(i,j+tank_column-1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i,j+tank_column-1) == "X") hit_count = 1
    IF (i-1 < nrows) grid(i-1,j+tank_column-1) = "*"
    CALL game_display()
    IF (bomb == 1) THEN
      CALL sleep(ptime2)
      DO l = -2, 2
      DO m = -2, 2
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -3, 3
      DO m = -3, 3
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -2, 2
      DO m = -2, 2
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -4, 4
      DO m = -4, 4
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -3, 3
      DO m = -3, 3
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -5, 5
      DO m = -5, 5
        IF (grid(i+l,j+tank_column+m) == "X") hit_count = 1
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -4, 4
      DO m = -4, 4
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display()
    END IF
    EXIT
  ELSE IF (grid(i,j+tank_column) == "X") THEN
    grid(i,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i-1 /= nrows) grid(i-1,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i-1 /= nrows) grid(i-1,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i /= nrows) grid(i,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i+1 /= nrows) grid(i+1,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i+1 /= nrows) grid(i+1,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i+1 /= nrows) grid(i+1,j+tank_column-1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i /= nrows) grid(i,j+tank_column-1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (i-1 /= nrows) grid(i-1,j+tank_column-1) = "*"
    CALL game_display()
    hit_count = 1
    IF (bomb == 1) THEN
      CALL sleep(ptime2)
      DO l = -2, 2
      DO m = -2, 2
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -3, 3
      DO m = -3, 3
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -2, 2
      DO m = -2, 2
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -4, 4
      DO m = -4, 4
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -3, 3
      DO m = -3, 3
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -5, 5
      DO m = -5, 5
        IF (i+l < nrows) grid(i+l,j+tank_column+m) = "*"
      END DO
    END DO
    CALL game_display()
    CALL sleep(ptime2)
    DO l = -4, 4
      DO m = -4, 4
        IF (grid(i+l,j+tank_column+m) == "*") grid(i+l,j+tank_column+m) = " "
      END DO
    END DO
    CALL game_display()
    END IF
    EXIT
  END IF
  ELSE IF (i == (nrows-1) .AND. j+tank_column > 1 .AND. j+tank_column < ncolumns) THEN
    grid(i,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i-1,j+tank_column) == "X") hit_count = 1
    grid(i-1,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i-1,j+tank_column+1) == "X") hit_count = 1
    grid(i-1,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i,j+tank_column+1) == "X") hit_count = 1
    grid(i,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i-1,j+tank_column) == "X") hit_count = 1
    grid(i-1,j+tank_column) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i-1,j+tank_column+1) == "X") hit_count = 1
    grid(i-1,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i,j+tank_column+1) == "X") hit_count = 1
    grid(i,j+tank_column+1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i,j+tank_column-1) == "X") hit_count = 1
    grid(i,j+tank_column-1) = "*"
    CALL game_display()
    CALL sleep(ptime)
    IF (grid(i-1,j+tank_column-1) == "X") hit_count = 1
    grid(i-1,j+tank_column-1) = "*"
    CALL game_display()
    EXIT
  END IF
END DO
l = 0
k = 0
DO i = 1, nrows
DO j = 1, ncolumns
  IF (grid(i,j) == "." .OR. grid(i,j) == "*") grid(i,j) = " "
  IF (grid(i,j) == "X") l = l + 1
  IF (grid(i,j) == "T") k = 1
END DO
END DO
IF (l == 0) win = 1
IF (k == 0) win = -1
WRITE (*,*)
IF (win == -1) THEN
  WRITE (*,*) "*KABOOM*"
ELSE
  IF (hit_count == 0) WRITE (*,*) "~MISS~"
  IF (hit_count == 1) WRITE (*,*) "HIT!"
END IF
print '(A)', "press Enter to continue"
READ (*,*)
END SUBROUTINE projectile_simulation


SUBROUTINE game_display()

CHARACTER(4) :: wind_display
integer :: i,j

print '(/,/,/,/,/,/,/)'
DO i = 1, nrows
  WRITE (*,*) (grid(i,j), j = 1, ncolumns)
END DO
IF (wind_direction == 0) wind_display = "WEST"
IF (wind_direction == 1) wind_display = "EAST"
score = 100.99999 - (0.333*(50-n_shots))**1.637
IF (bomb == 0) THEN
  WRITE (*,*)
  WRITE (*,18) angle, velocity, wind_speed, wind_display, n_shots, score
  18 FORMAT (1X,"Angle: ",I3,3X,"Velocity: ",I3,3X,"Wind: ",I2,1X,A4,3X,"Shots Remaining: ",I2,3X,"Current Score: ",I3)
ELSE
  WRITE (*,*)
  WRITE (*,180) angle, velocity, wind_speed, wind_display, n_shots, score, "B-O-M-B"
  180 FORMAT (1X,"Angle: ",I3,3X,"Velocity: ",I3,3X,"Wind: ",I2,1X,A4,3X,"Shots Remaining: ",I2,3X,"Current Score: ",I3,5X,A7)
END IF
END SUBROUTINE game_display

end module game
