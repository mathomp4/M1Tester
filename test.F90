module leapmod

   implicit none
   
   contains

      logical function is_leap_year(year)

         integer, intent(in) :: year

         is_leap_year = mod(year,4) == 0 .and. ( mod(year,100) /= 0  .or. mod(year,400) == 0 )

      end function is_leap_year

end module leapmod
      
program test

   use leapmod
   use iso_fortran_env
   implicit none

   character(len=6) :: year_string
   integer :: year, count

   count = command_argument_count()
   write(*,*) count
   call get_command_argument(1,year_string)
   read(year_string,*) year

   write(output_unit,'(A,1X,I6)') "Year:", year
   write(output_unit,'(A,1X,L6)') "Leap:", is_leap_year(year)

end program test

