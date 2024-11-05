program Two_Circles
    ! x1, y1 : 1つ目の円の中心座標
    ! r1 : 1つ目の円の半径
    ! x2, y2 : 2つ目の円の中心座標
    ! r2 : 2つ目の円の半径
    ! dx, dy : 2つの円の中心のx, y座標の差
    ! dist_squared : 2つの円の中心間の距離の2乗
    ! dist : 2つの円の中心間の距離
    ! r_sum : 2つの円の半径の和
    ! r_diff : 2つの円の半径の差

    implicit none
    integer(16) :: x1, y1, r1, x2, y2, r2
    real(16) :: dx, dy, dist_squared, dist
    real(16) :: r_sum, r_diff

    ! 入力
    read (*, *) x1, y1, r1
    read (*, *) x2, y2, r2

    ! 2つの円の中心間の距離を計算
    dx = real(x2 - x1, 16)
    dy = real(y2 - y1, 16)
    dist_squared = dx*dx + dy*dy
    dist = sqrt(dist_squared)

    ! 半径の和と差
    r_sum = real(r1 + r2, 16)
    r_diff = abs(real(r1 - r2, 16))

    ! 位置関係を判定して結果を出力
    if (dist < r_diff) then
        print *, 1 ! 完全に含まれていて接していない
    elseif (dist == r_diff) then
        print *, 2 ! 完全に含まれていて接している
    elseif (dist < r_sum) then
        print *, 3 ! 交差している
    elseif (dist == r_sum) then
        print *, 4 ! 接している
    else
        print *, 5 ! 接しておらず、共通部分がない
    end if

end program Two_Circles
