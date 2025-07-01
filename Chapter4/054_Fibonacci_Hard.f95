program Fibonacci_Hard
    ! N           : 求めたいフィボナッチ数の項番号 (3 ≤ N ≤ 10^18)
    ! answer_pair : モジュール fib_mod の fib 関数が返す (F(N), F(N+1))
    ! answer      : F(N) mod 10^9 を保持

    use, intrinsic :: iso_fortran_env, only: int64
    use fib_mod
    implicit none
    integer(int64) :: N
    type(FibPair) :: answer_pair
    integer(int64) :: answer

    ! 入力
    read (*, *) N

    ! フィボナッチ計算（高速二分法）
    answer_pair = fib(N)
    answer = answer_pair%f0

    ! 結果の出力
    print *, answer

end program Fibonacci_Hard

module fib_mod
    ! フィボナッチ数列モジュール：高速二分法で O(log N) 計算
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    ! MODV: 計算を行う法 (10^9)
    integer(int64), parameter :: MODV = 1000000000_int64

    ! FibPair: fib 関数の結果格納型
    type :: FibPair
        ! f0: F(n) mod MODV
        integer(int64) :: f0
        ! f1: F(n+1) mod MODV
        integer(int64) :: f1
    end type FibPair

contains
    recursive function fib(n) result(r)
        ! fib: 高速二分法によるフィボナッチ数の計算
        integer(int64), intent(in) :: n
        type(FibPair) :: r, t
        integer(int64) :: a, b, c, d

        if (n == 0_int64) then
            ! F(0)=0, F(1)=1
            r%f0 = 0_int64
            r%f1 = 1_int64
        else
            ! 半分の項を再帰計算
            t = fib(n/2_int64)
            a = t%f0 ! F(k)
            b = t%f1 ! F(k+1)

            ! c = F(2k)   = F(k)*[2*F(k+1) − F(k)]
            c = mod(a*mod(2_int64*b - a + MODV, MODV), MODV)
            ! d = F(2k+1) = F(k)^2 + F(k+1)^2
            d = mod(a*a + b*b, MODV)

            if (mod(n, 2_int64) == 0_int64) then
                r%f0 = c
                r%f1 = d
            else
                ! 奇数番目なら一つシフト
                r%f0 = d
                r%f1 = mod(c + d, MODV)
            end if
        end if
    end function fib
end module fib_mod
