program Climb_Stairs
    ! N  : 階段の数
    ! DP : 階段を i 段目まで登るパターン数
    implicit none
    integer(16) N, DP(100), i

    ! 入力
    read (*, *) N

    ! DP
    DP(1) = 1
    DP(2) = 1
    do i = 3, N + 1
        DP(i) = DP(i - 1) + DP(i - 2)
    end do

    ! 結果の出力
    write (*, *) DP(N + 1)
end program Climb_Stairs
