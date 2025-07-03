program Sum_of_Difference_Easy
    ! 概要
    ! このプログラムは、昇順に並んだ N 個の整数 A(1)～A(N) に対して、
    ! i = 1 から N まで、j = i+1 から N までの (A(j) - A(i)) の総和を求める。
    ! 式変形により、各 A(i) が何回「引かれ」、何回「足される」かを考えて高速に計算する。
    ! 計算式：sum += A(i) * (2*i - N - 1)（i=1..N）
    ! N は最大 200000 なので、O(N) で処理できる必要がある。

    ! N       : 配列 A の要素数（整数列の長さ）
    ! A(maxn) : 昇順に並んだ整数列 A を格納する配列（最大 200000 要素）
    ! result  : 条件を満たすすべての (A(j) - A(i)) の総和（最終的な出力値）
    implicit none
    integer, parameter :: maxn = 200000
    integer :: N, i
    integer(8) :: A(maxn)
    integer(8) :: result

    ! 入力
    read (*, *) N
    read (*, *) (A(i), i=1, N)
    result = 0

    ! 各 A(i) の寄与を加算
    do i = 1, N
        result = result + A(i)*int(2*i - N - 1, 8)
    end do

    ! 結果の出力
    print *, result

end program Sum_of_Difference_Easy
