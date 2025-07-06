program Simple_Math
    ! 概要:
    ! 与えられた正整数 A, B, C に対して
    ! Σ_{a=1}^A Σ_{b=1}^B Σ_{c=1}^C (a*b*c) を求める。
    ! 等差数列の和を利用して O(1) で計算する。
    ! 答えは 998244353 で割った余りを出力する。

    ! A,B,C   : 入力された3つの整数
    ! MODULO  : 998244353（法）
    ! sa,sb,sc: それぞれ A,B,C の和
    ! ans     : 求める答え
    implicit none
    integer(8) :: A, B, C
    integer(8), parameter :: MODULO = 998244353
    integer(8) :: sa, sb, sc, ans

    ! 入力
    read (*, *) A, B, C

    ! それぞれの和を計算
    sa = MOD(A*(A + 1)/2, MODULO)
    sb = MOD(B*(B + 1)/2, MODULO)
    sc = MOD(C*(C + 1)/2, MODULO)

    ! 全体の積を計算して MOD を取る
    ans = MOD(sa*sb, MODULO)
    ans = MOD(ans*sc, MODULO)

    ! 結果の出力
    print *, ans

end program Simple_Math
