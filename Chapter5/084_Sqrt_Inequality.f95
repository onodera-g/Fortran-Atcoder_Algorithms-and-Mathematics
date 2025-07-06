program Sqrt_Inequality
    ! 概要:
    ! 与えられた3つの整数 a, b, c について、次の条件を判定するプログラム。
    ! 条件1: a+b-c > 0 なら No
    ! 条件2: (a+b-c)^2 - 4*a*b > 0 なら Yes
    ! 上記のどちらも満たさない場合は No と出力する。

    ! a : 整数（入力値、長さのようなもの）
    ! b : 整数（入力値、長さのようなもの）
    ! c : 整数（入力値、長さのようなもの）
    ! s : 中間計算用の整数、a+b-c の値を格納

    implicit none
    integer(8) :: a, b, c
    integer(8) :: s

    ! 入力
    read (*, *) a, b, c

    ! 計算
    s = a + b - c

    ! 判定
    if (s > 0) then
        print *, "No"
    else if (s*s - 4*a*b > 0) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program Sqrt_Inequality
