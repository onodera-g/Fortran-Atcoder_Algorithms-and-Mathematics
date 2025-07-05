program Pyramid
    ! 概要
    ! このプログラムは、N 段のピラミッドの最下段に整数列 A(1)〜A(N) が書かれているとき、
    ! 上の段を「隣り合う2つを加算して次の段にする」操作を繰り返して、
    ! 最上段に書かれる値を 10^9+7 で割った余りで求める。
    ! 二項係数 C(N-1,i) を用いると計算できるため、
    ! モジュラ逆数を用いて計算する。

    ! 変数の説明
    ! N               : ピラミッドの段数（最下段の要素数）
    ! A(N)            : 最下段の整数列
    ! i               : ループ変数
    ! result          : 最上段の値（最終結果）
    ! comb            : 二項係数 C(N-1, i)
    ! MODULUS         : 計算の法 10^9+7

    implicit none
    integer(8), parameter :: MODULUS = 1000000007_8
    integer(8) :: N, i, result, comb
    integer(8), allocatable :: A(:)

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)

    result = 0_8
    comb = 1_8

    ! 最上段の値を計算
    do i = 1, N
        ! 現在の項を加算
        result = modulo_safe(result + modulo_safe(comb*A(i), MODULUS), MODULUS)

        ! comb = comb * (N-i)/i を更新
        if (i < N) then
            comb = modulo_safe(comb*(N - i), MODULUS)
            comb = modulo_safe(comb*modinv(i, MODULUS), MODULUS)
        end if
    end do

    ! 結果の出力
    print *, result

contains

    ! 正の余りを返す安全な mod 計算
    integer(8) function modulo_safe(a, m)
        integer(8), intent(in) :: a, m
        modulo_safe = a - (a/m)*m
        if (modulo_safe < 0) modulo_safe = modulo_safe + m
    end function modulo_safe

    ! a の逆元を求める（フェルマーの小定理）
    integer(8) function modinv(a, m)
        integer(8), intent(in) :: a, m
        modinv = modpow(a, m - 2_8, m)
    end function modinv

    ! a^b mod m を計算（二分累乗法）
    integer(8) function modpow(a, b, m)
        integer(8), intent(in) :: a, b, m
        integer(8) :: res, base, exp
        res = 1_8
        base = modulo_safe(a, m)
        exp = b
        do while (exp > 0_8)
            if (modulo_safe(exp, 2_8) == 1_8) res = modulo_safe(res*base, m)
            base = modulo_safe(base*base, m)
            exp = exp/2_8
        end do
        modpow = res
    end function modpow

end program Pyramid
