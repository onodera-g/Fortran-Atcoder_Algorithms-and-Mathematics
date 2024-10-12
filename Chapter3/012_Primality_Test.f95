program Primality_Test
    ! N    : 素数判定する整数
    ! flag : 結果の出力分岐用(false:素数でない)
    implicit none
    integer(8) N, i
    logical:: flag = .true.

    ! 入力
    read (*, *) N

    ! 素数判定
    do i = 2, int(sqrt(real(N)))
        if (mod(N, i) == 0) then
            flag = .false.
            exit
        end if
    end do

    ! 結果の出力
    if (flag) then
        write (*, *) 'Yes'
    else
        write (*, *) 'No'
    end if
end program Primality_Test
