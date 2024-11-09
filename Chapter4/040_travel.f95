program travel
    ! N: 駅の数
    ! M: 太郎君が通過する駅の数
    ! A: 各駅間の距離 (キロメートル) を格納する配列
    ! B: 太郎君が通過する駅の順番を格納する配列
    ! sum: 各駅までの累積距離を格納する配列
    ! now: 現在の駅番号
    ! dest: 次の目的地の駅番号
    ! ans: 累計移動距離 (メートル単位) の合計
    implicit none
    integer(16) i
    integer(16) N, M
    integer(16) A(200000), B(200000), sum(200000), now, dest, ans

    ! 入力
    read (*, *) N
    read (*, *) A(1:N - 1)
    read (*, *) M
    do i = 1, M
        read (*, *) B(i)
    end do

    ! 累積距離の計算
    sum = 0
    sum(1) = 0
    do i = 2, N
        sum(i) = sum(i - 1) + A(i - 1) ! 各駅までの累積距離を計算
    end do

    ! 累計移動距離の計算
    ans = 0
    do i = 1, M - 1
        now = B(i) ! 現在の駅
        dest = B(i + 1) ! 次の目的地の駅
        ans = ans + abs(sum(dest) - sum(now)) ! 移動距離を累計
    end do

    ! 結果の出力
    write (*, *) ans

end program travel
