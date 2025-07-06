program Log_Inequality_2
    ! 概要:
    ! 与えられた正の整数 a,b,c について log2(a) < b*log2(c) か判定する。
    ! 対数の性質から a < c^b を判定すれば良い。
    ! c^b が大きくなりすぎる場合は a < c^b が必ず成り立つ。

    ! a,b,c : 入力された整数
    ! cur   : 累乗計算用の変数
    ! i     : ループカウンタ
    ! INF   : 10^18+1 （オーバーフロー判定用）

    implicit none
    integer(8) :: a, b, c, i
    integer(8) :: cur
    integer(8), parameter :: INF = 10_8**18 + 1

    ! 入力
    read (*, *) a, b, c

    ! c = 1 の場合は c^b = 1 なので a < 1 なら Yes
    if (c == 1) then
        if (a < 1) then
            print *, "Yes"
        else
            print *, "No"
        end if
        stop
    end if

    ! c > 1 の場合
    cur = 1
    do i = 1, b
        if (cur > a/c) then
            ! 途中で a を超えたら Yes
            print *, "Yes"
            stop
        end if
        cur = cur*c
    end do

    ! 計算が終わった時点で a >= c^b
    if (a < cur) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program Log_Inequality_2
