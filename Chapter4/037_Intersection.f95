program Intersection
    ! x1, y1   : 1つ目の線分の始点の座標
    ! x2, y2   : 1つ目の線分の終点の座標
    ! x3, y3   : 2つ目の線分の始点の座標
    ! x4, y4   : 2つ目の線分の終点の座標
    ! intersect: 線分が交差するかどうかを示す論理変数
    implicit none
    integer(kind=8) :: x1, y1, x2, y2, x3, y3, x4, y4
    logical :: intersect

    ! 入力
    read (*, *) x1, y1
    read (*, *) x2, y2
    read (*, *) x3, y3
    read (*, *) x4, y4

    ! 交差判定
    intersect = do_intersect(x1, y1, x2, y2, x3, y3, x4, y4)

    ! 結果の出力
    if (intersect) then
        print *, "Yes"
    else
        print *, "No"
    end if

contains

    ! オリエンテーションを計算する関数
    ! xa, ya: 3点のうち最初の点の座標
    ! xb, yb: 3点のうち2番目の点の座標
    ! xc, yc: 3点のうち3番目の点の座標
    integer(kind=8) function orientation(xa, ya, xb, yb, xc, yc)
        integer(kind=8), intent(in) :: xa, ya, xb, yb, xc, yc
        integer(kind=8) :: val

        ! オリエンテーションの計算
        val = (xb - xa)*(yc - ya) - (yb - ya)*(xc - xa)

        if (val > 0) then
            orientation = 1 ! 時計回り
        else if (val < 0) then
            orientation = -1 ! 反時計回り
        else
            orientation = 0 ! 共線
        end if
    end function orientation

    ! 点が線分上にあるかを判定する関数
    logical function on_segment(xa, ya, xb, yb, xc, yc)
        ! xa, ya: 線分の始点の座標
        ! xb, yb: 線分の終点の座標
        ! xc, yc: 判定対象の点の座標
        integer(kind=8), intent(in) :: xa, ya, xb, yb, xc, yc

        ! 点Cが線分ABの範囲内にあるかを判定
        if (min(xa, xb) <= xc .and. xc <= max(xa, xb) .and. &
            min(ya, yb) <= yc .and. yc <= max(ya, yb)) then
            on_segment = .true.
        else
            on_segment = .false.
        end if
    end function on_segment

    ! 2つの線分が交差するかを判定する関数
    logical function do_intersect(x1, y1, x2, y2, x3, y3, x4, y4)
        ! x1, y1: 1つ目の線分の始点の座標
        ! x2, y2: 1つ目の線分の終点の座標
        ! x3, y3: 2つ目の線分の始点の座標
        ! x4, y4: 2つ目の線分の終点の座標
        integer(kind=8), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        integer(kind=8) :: o1, o2, o3, o4

        ! 各点のオリエンテーションを計算
        o1 = orientation(x1, y1, x2, y2, x3, y3)
        o2 = orientation(x1, y1, x2, y2, x4, y4)
        o3 = orientation(x3, y3, x4, y4, x1, y1)
        o4 = orientation(x3, y3, x4, y4, x2, y2)

        ! 一般的な交差の判定
        if (o1 /= o2 .and. o3 /= o4) then
            do_intersect = .true.
            return
        end if

        ! 特殊ケースの処理（共線の場合）
        if (o1 == 0 .and. on_segment(x1, y1, x2, y2, x3, y3)) then
            do_intersect = .true.
            return
        end if
        if (o2 == 0 .and. on_segment(x1, y1, x2, y2, x4, y4)) then
            do_intersect = .true.
            return
        end if
        if (o3 == 0 .and. on_segment(x3, y3, x4, y4, x1, y1)) then
            do_intersect = .true.
            return
        end if
        if (o4 == 0 .and. on_segment(x3, y3, x4, y4, x2, y2)) then
            do_intersect = .true.
            return
        end if

        ! 交差しない場合
        do_intersect = .false.
    end function do_intersect

end program Intersection
