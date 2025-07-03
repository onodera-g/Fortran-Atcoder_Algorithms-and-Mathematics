program Sum_of_Maximum
    ! 概要
    ! このプログラムは、昇順に並んだ N 個の整数 A(1) ～ A(N) から 1個以上の部分集合を選び、
    ! 各部分集合の「最大値」を合計した値を MOD=1000000007 で割った余りとして出力する。
    ! 各 A(i) は、それ以降の要素を含む 2**(i-1) 個の部分集合で最大値になる。
    ! よって、sum = Σ A(i) * 2**(i-1) を MOD を使って計算する。

    ! N            : 配列の長さ
    ! A(300000)    : 昇順に並んだ整数の配列
    ! i            : ループ変数
    ! pow2         : 2 の i−1 乗を保持（mod MOD）
    ! result       : 最終的な出力値
    ! MOD          : 法 1000000007

    implicit none
    integer(8) :: N, i
    integer(8), allocatable :: A(:)
    integer(8) :: pow2, result, temp1
    integer(8), parameter :: MOD = 1000000007

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    pow2 = 1
    result = 0

    ! 各要素に対して、最大値として貢献する回数分（2**(i-1)) 掛けた値を加算
    do i = 1, N
        temp1 = A(i)*pow2
        temp1 = temp1 - (temp1/MOD)*MOD ! mod 相当処理（整数同士の演算で安全）
        result = result + temp1
        if (result >= MOD) result = result - MOD
        pow2 = pow2*2
        if (pow2 >= MOD) pow2 = pow2 - MOD
    end do

    ! 結果の出力
    print *, result
end program Sum_of_Maximum
