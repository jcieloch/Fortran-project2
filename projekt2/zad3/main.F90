program main
  use zad1
  use cache
  use cache_dot_prod
  use dot_prod
  implicit none
  integer (kind = 4), parameter :: N = 20
  real (kind = 8), allocatable, dimension(:,:) :: first, second, multiply
  integer (kind = 4) :: args(N), i, j, status
  real (kind = 8) :: times(5, N)
  real (kind = 8) :: time_start, time_stop

  j=100
  do i = 1, N
    args(i) = j
    allocate(first(j, j))
    allocate(second(j, j))
    allocate(multiply(j, j))
    multiply = 0.d0
    call CPU_TIME(time_start)
    call mm_cache(first,second,multiply,status)
    call CPU_TIME(time_stop)
    times(1, i) = time_stop - time_start

    call CPU_TIME(time_start)
    call mm_zad1(first,second,multiply,status)
    call CPU_TIME(time_stop)
    times(2, i) = time_stop - time_start

    call CPU_TIME(time_start)
    call mm_dot_prod(first,second,multiply,status)
    call CPU_TIME(time_stop)
    times(3, i) = time_stop - time_start

    call CPU_TIME(time_start)
    call mm_cache_dot_prod(first,second,multiply,status)
    call CPU_TIME(time_stop)
    times(4, i) = time_stop - time_start

    call CPU_TIME(time_start)
    multiply = matmul(first,second)
    call CPU_TIME(time_stop)
    times(5, i) = time_stop - time_start

    deallocate(first, second, multiply)
    j = j+100
    print *, j
  end do

  open(unit=1, file='data.dat')
  do i=1, 5
    do j=1, N
      write(1, *) times(i, j), args(j), i
    end do
  end do
  close(unit=1)

end program
