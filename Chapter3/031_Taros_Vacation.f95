program Taros_Vacation
    ! N  : 夏休みの日数
    ! A  : 各日で上昇する実力
    ! DP : その日までの実力の上昇量の最大値
    implicit none
    integer(8) :: i, N, A(500000), DP(500000) = 0

    ! 入力
    read (*, *) N
    read (*, *) A(1:N)

    ! DP
    DP = 0
    DP(1) = A(1)
    DP(2) = A(2)
    do i = 3, N
        ! 2個前と3個前を比較してでかいほうをとる。
        ! 4個前以降は2個前と3個前をさらに足せるので比較の意味はない
        DP(i) = maxval(DP(i - 3:i - 2)) + A(i)
    end do

    ! 結果の出力
    write (*, *) MAXVAL(DP)
end program Taros_Vacation
