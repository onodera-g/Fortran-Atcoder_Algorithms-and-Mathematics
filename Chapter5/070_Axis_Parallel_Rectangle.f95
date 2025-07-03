
program Axis_Parallel_Rectangle
    ! -- 概要 --
    ! このプログラムは、N個の点の中からK個以上を含む長方形を全探索し、
    ! X軸Y軸に平行な最小の面積を持つ長方形を見つけ出す。
    ! 長方形は辺上の点も内部に含むとする。
    ! 点数 N が最大50なので、座標の組み合わせ全通りを調べることが可能。

    ! N              : 点の総数
    ! K              : 含む必要のある最小の点数
    ! x(50), y(50)   : 各点のX座標とY座標
    ! i, j, a, b     : 座標ペア選択用のループ変数
    ! p              : 点のチェック用のループ変数
    ! xmin, xmax     : 候補長方形のX座標の最小と最大
    ! ymin, ymax     : 候補長方形のY座標の最小と最大
    ! cnt            : 候補長方形に含まれる点の数
    ! area           : 面積
    ! min_area       : 最小面積の記録（初期値は十分大きくする）

    implicit none
    integer, parameter :: INF = 2000000000
    integer :: N, K
    integer :: x(50), y(50)
    integer :: i, j, a, b, p, cnt
    integer(8) :: xmin, xmax, ymin, ymax
    integer(8) :: area, min_area

    ! 入力
    read (*, *) N, K
    do i = 1, N
        read (*, *) x(i), y(i)
    end do

    min_area = 9223372036854775807_8 ! 64bit最大値で初期化

    ! 各 x 座標と y 座標のペアで長方形を決める
    do i = 1, N
        do j = 1, N
            xmin = min(x(i), x(j))
            xmax = max(x(i), x(j))
            do a = 1, N
                do b = 1, N
                    ymin = min(y(a), y(b))
                    ymax = max(y(a), y(b))

                    cnt = 0
                    ! この長方形に含まれる点をカウント
                    do p = 1, N
                        if (x(p) >= xmin .and. x(p) <= xmax .and. &
                            y(p) >= ymin .and. y(p) <= ymax) then
                            cnt = cnt + 1
                        end if
                    end do

                    ! K点以上含むなら面積を計算して最小更新
                    if (cnt >= K) then
                        area = (xmax - xmin)*(ymax - ymin)
                        if (area < min_area) then
                            min_area = area
                        end if
                    end if
                end do
            end do
        end do
    end do

    ! 結果の出力
    print *, min_area

end program Axis_Parallel_Rectangle
