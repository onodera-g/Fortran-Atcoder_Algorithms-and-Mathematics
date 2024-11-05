program Nearest_Points
    ! N : 点の数
    ! x, y : 各点の座標を格納する配列
    ! i, j : 2点を表すインデックス
    ! dx, dy : 2つの点のx, y座標の差
    ! dist : 2つの点の間の距離
    ! min_dist : 最も近い2つの点の距離の最小値

    implicit none
    integer(16) :: N, i, j
    real(16) :: dx, dy, dist, min_dist
    integer(16), allocatable :: x(:), y(:)

    ! 入力
    read (*, *) N
    allocate (x(N), y(N))

    ! 各点の座標の入力
    do i = 1, N
        read (*, *) x(i), y(i)
    end do

    ! 初期化：最初の2点間の距離を最小値に設定
    min_dist = 1.0e18_16
    do i = 1, N - 1
        do j = i + 1, N
            dx = real(x(j) - x(i), 16)
            dy = real(y(j) - y(i), 16)
            dist = sqrt(dx*dx + dy*dy)
            if (dist < min_dist) then
                min_dist = dist
            end if
        end do
    end do

    ! 結果の出力
    write (*, '(F24.15)') min_dist
end program Nearest_Points
