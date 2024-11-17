program Easy_Graph_Problem
    implicit none
    ! N  : 頂点数
    ! M  : 辺の数
    ! a, b : 各辺の頂点番号を格納する変数
    ! cnt: 条件を満たす頂点数をカウントする変数
    ! num_lower : 各頂点における自分より小さい隣接頂点の数を格納する配列
    integer(16) :: N, M, a, b, i, cnt
    integer(16), allocatable :: num_lower(:)

    ! 入力
    read (*, *) N, M
    allocate (num_lower(N))
    do i = 1, M
        read (*, *) a, b
        if (a < b) then
            num_lower(b) = num_lower(b) + 1
        else
            num_lower(a) = num_lower(a) + 1
        end if
    end do

    ! 条件を満たす頂点のカウント
    cnt = 0
    do i = 2, N
        if (num_lower(i) == 1) then
            cnt = cnt + 1
        end if
    end do

    ! 結果を出力
    print '(i0)', cnt
end program Easy_Graph_Problem
