program ModSum_Max
    ! 概要:
    ! 整数 N に対して {1,…,N} の順列 {P1,…,PN} を選び、
    ! 各 i について i mod Pi の総和の最大値を計算する。
    ! Pi を大きい順に割り当てることで、総和は (N*(N-1))/2 となる。

    ! N    : 入力として与えられる整数 N
    ! ans  : 計算結果（総和の最大値）

    implicit none

    integer(8) :: N
    integer(8) :: ans

    ! 入力
    read (*, *) N

    ! 計算
    ans = N*(N - 1)/2

    ! 結果の出力
    print *, ans
end program ModSum_Max
