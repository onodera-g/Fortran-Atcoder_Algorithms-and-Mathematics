program Move_On_Squares_1
    ! --概要--
    ! 操作回数が 奇数 なら、移動総数も奇数でないとたどり着けない。
    ! 移動方向は上下左右なので、到達可能な位置 (X, Y) への最短距離は |X| + |Y|。
    ! この値が N 以下で、かつ N と同じ偶奇であれば、たどり着ける。

    ! N        : 操作回数（ちょうど N 回動かす必要がある）
    ! X, Y     : 目的地の座標
    ! dist     : 始点 (0,0) から目的地 (X,Y) までのマンハッタン距離（|X| + |Y|）
    implicit none
    integer :: N, X, Y
    integer :: dist

    ! 入力
    read (*, *) N, X, Y

    ! マンハッタン距離の計算
    dist = abs(X) + abs(Y)

    ! 移動可能かどうかの判定
    if (dist <= N .and. mod(dist, 2) == mod(N, 2)) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program Move_On_Squares_1
