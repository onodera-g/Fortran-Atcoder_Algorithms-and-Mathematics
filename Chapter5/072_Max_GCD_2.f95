program Max_GCD_2
    ! 概要
    ! このプログラムは、区間 [A, B] 内の整数 x, y に対して A ≤ x < y ≤ B を満たすペアの中で、
    ! 最大公約数 gcd(x, y) の最大値を求める。
    ! 方針としては、d を B から 1 まで下げながら、
    ! 区間内に d の倍数が2つ以上存在するかを調べ、存在すればその d が最大の gcd になる。

    ! A, B          : 入力として与えられる整数区間の下限と上限
    ! d             : 現在調査中の最大公約数の候補
    ! lower, upper  : 区間 [A,B] における d の倍数の最小値と最大値
    ! result        : 最終的に求める最大の gcd

    implicit none
    integer :: A, B, d
    integer :: lower, upper, result

    ! 入力
    read (*, *) A, B

    result = 1

    ! d を B から 1 まで調べる（大きい順に）
    do d = B, 1, -1
        lower = ((A + d - 1)/d)*d ! A以上の最小のdの倍数
        upper = (B/d)*d ! B以下の最大のdの倍数
        if (lower < upper) then ! 倍数が2つ以上あるかチェック
            result = d
            exit
        end if
    end do

    ! 結果の出力
    print *, result
end program Max_GCD_2
