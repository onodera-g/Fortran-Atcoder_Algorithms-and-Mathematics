program Jiros_Vacation
    ! N   : 休みの日数
    ! A   : 1,2が出た場合の勉強時間
    ! B   : 3,4,5,6が出た場合の勉強時間
    ! ans : 期待値
    implicit none
    integer N, i
    real(16) ans, A(200000), B(200000)

    ! 入力
    read (*, *) N
    read (*, *) (A(i), i=1, N)
    read (*, *) (B(i), i=1, N)

    ! 期待値の計算
    ans = 0
    do i = 1, N
        ans = ans + (2*A(i) + 4*B(i))/6
    end do

    ! 結果の出力
    write (*, *) ans

end program Jiros_Vacation
