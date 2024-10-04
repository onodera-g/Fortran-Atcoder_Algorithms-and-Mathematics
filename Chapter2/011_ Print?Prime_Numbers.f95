program Print_Prime_Numbers
    ! N            : 素数を求める範囲の上限
    ! prime_number : 素数の可否を管理
    implicit none
    integer N, i, j
    logical prime_number(3000)

    read (*, *) N

    ! エラトステネスの篩で素数を計算
    prime_number = .true.
    do i = 2, int(sqrt(real(N)))
        if (prime_number(i)) then
            do j = i*i, N, i
                prime_number(j) = .false.
            end do
        end if
    end do

    ! 結果の出力
    do i = 2, N
        if (prime_number(i)) write (*, '(i0,1x)', advance='no') i
    end do

end program Print_Prime_Numbers

