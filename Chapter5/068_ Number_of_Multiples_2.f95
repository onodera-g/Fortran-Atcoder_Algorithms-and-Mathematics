program Number_of_Multiples_2
    ! -- 概要 --
    ! このプログラムは、1以上N以下の整数の中から、
    ! 少なくとも1つ以上の与えられたK個の数 V(1)...V(K) の倍数であるものの個数を求める。
    ! アプローチとしては包除原理を使用し、すべての部分集合に対して最小公倍数を求めて数え上げる。
    ! 条件を満たす数の個数は、奇数サイズの部分集合については加算、偶数サイズの部分集合については減算する。
    ! なお、数Nは最大10^12まで取り得るため、すべての処理は64ビット整数（integer(8)）で行う。

    ! N              : 上限値。1以上N以下の整数を対象とする
    ! K              : 倍数の基準となる整数の個数
    ! V(10)          : 倍数の基準となる値を格納する配列（最大10個）
    ! i              : ループ変数（基準整数を選ぶ）
    ! subset_mask    : 現在の部分集合を表すビットマスク（整数）
    ! lcm_value      : 現在の部分集合の最小公倍数
    ! count          : 答えとして出力する、条件を満たす整数の個数
    ! bits           : 現在の部分集合に含まれる要素数（偶奇で符号が変わる）

    implicit none
    integer :: K, i, bits
    integer(4) :: subset_mask
    integer(8) :: N, count, lcm_value
    integer(8) :: V(10)

    ! 入力
    read (*, *) N, K
    read (*, *) (V(i), i=1, K)

    count = 0

    ! すべての部分集合について包除原理を適用（空集合は除外）
    do subset_mask = 1, 2**K - 1
        lcm_value = 1
        bits = 0

        do i = 1, K
            if (iand(subset_mask, 2**(i - 1)) /= 0) then
                bits = bits + 1
                lcm_value = lcm(lcm_value, V(i))
                if (lcm_value > N) exit
            end if
        end do

        if (mod(bits, 2) == 1) then
            count = count + N/lcm_value
        else
            count = count - N/lcm_value
        end if
    end do

    ! 結果の出力
    print *, count

contains

    ! 最小公倍数を求める関数
    integer(8) function lcm(a, b)
        integer(8), intent(in) :: a, b
        integer(8) :: g
        g = gcd(a, b)
        lcm = a/g*b
    end function lcm

    ! 最大公約数を求める関数（ユークリッドの互除法）
    integer(8) function gcd(a_in, b_in)
        integer(8), intent(in) :: a_in, b_in
        integer(8) :: a, b, temp
        a = a_in
        b = b_in
        do while (b /= 0)
            temp = b
            b = mod(a, b)
            a = temp
        end do
        gcd = a
    end function gcd

end program Number_of_Multiples_2
