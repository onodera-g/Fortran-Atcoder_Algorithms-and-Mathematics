program Frog1
    ! N  : 足場の数
    ! h  : 足場の高さ
    ! DP : それぞれの足場に移動するまでに使う体力の最小値
    implicit none
    integer(16) N, DP(1000000), i, h(1000000), tmp1, tmp2

    ! 入力
    read (*, *) N
    read (*, *) (h(i), i=1, N)

    ! DP
    DP(1) = 0
    DP(2) = abs(h(1) - h(2))
    do i = 3, N
        tmp1 = DP(i - 1) + abs(h(i - 1) - h(i)) ! 隣から移動してくる場合
        tmp2 = DP(i - 2) + abs(h(i - 2) - h(i)) ! 1つ飛ばしで移動する場合
        DP(i) = min(tmp1, tmp2)
    end do

    ! 結果の出力
    write (*, *) DP(N)
end program Frog1
