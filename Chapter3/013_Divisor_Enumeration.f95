program Divisor_Enumeration
    ! N : 約数を調べたい整数
    implicit none
    integer(16) N, i

    ! 入力
    read (*, *) N

    ! 約数列挙
    i = 1
    do while (i*i < N)
        if (mod(N, i) == 0) then
            write (*, *) i
            write (*, *) N/i
        end if
        i = i + 1
    end do

end program Divisor_Enumeration
