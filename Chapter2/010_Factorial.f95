program Factorial
    ! N   : N!を求める数
    ! sum : N!の答え
    implicit none
    integer(16) N, sum, i

    ! 入力
    read (*, *) N

    ! N! の計算
    sum = 1
    do i = 2, N
        sum = sum*i
    end do

    ! 結果の出力
    write (*, *) sum

end program
