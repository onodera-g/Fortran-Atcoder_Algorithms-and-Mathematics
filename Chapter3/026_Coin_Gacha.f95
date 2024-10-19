program Coin_Gacha
    implicit none
    ! N: コインの種類の数
    ! ans: 期待値の計算結果を格納する変数
    integer N, i
    real(16) ans

    ! 入力
    read (*, *) N

    ! 期待値を計算
    ans = 0
    do i = 1, N
        ans = ans + N/(real(i, 16))
    end do

    ! 結果の出力
    print *, ans

end program Coin_Gacha

