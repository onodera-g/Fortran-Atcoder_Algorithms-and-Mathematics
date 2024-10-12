program Combination_Easy
    ! n : nCr の n
    ! r : nCr の r
    implicit none
    integer(16) n, r, tmp1, tmp2, i

    ! 入力
    read (*, *) n, r

    ! nCr の計算
    tmp1 = 1; tmp2 = 1
    do i = n, n - r + 1, -1 !分子
        tmp1 = i*tmp1
    end do
    do i = 1, r
        tmp2 = tmp2*i
    end do

    ! 結果の出力
    write (*, *) tmp1/tmp2
end program Combination_Easy
