module dot_prod
contains
  subroutine mm_dot_prod(first, second, multiply, status)
    implicit none
    real (kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
    real (kind = 8), intent(in) :: second(: ,:) ! druga macierz
    real (kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
    integer (kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK
    integer (kind = 4) :: a(2), b(2), c(2), i, j ! rozmiary macierzy

    a = shape(first)
    b = shape(second)
    c = shape(multiply)
    if (a(2) /= b(1)) then
      status = 1
    else if (a(1) /= c(1) .or. b(2) /= c(2)) then
      status = 2
    else
      do j = 1, b(2)
        do i = 1, a(1)
          multiply(i,j)=dot_product(first(i,:),second(:,j))
        end do
      end do
      status = 0
    end if
  end subroutine
end module
