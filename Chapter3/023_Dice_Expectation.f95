program Dice_Expectation
    ! N   : サイコロの数
    ! A   : 赤のサイコロの目
    ! B   : 青のサイコロの目
    ! ans : 期待値
    implicit none
    integer(16) N, i
    real(16) B(100000), A(100000), ans

    ! 入力
    read (*, *) N
    read (*, *) (B(i), i=1, N)
    read (*, *) (A(i), i=1, N)

    ! 期待値の計算
    ans = sum(B)/N + sum(A)/N

    ! 結果の出力
    write (*, *) ans

end program Dice_Expectation
