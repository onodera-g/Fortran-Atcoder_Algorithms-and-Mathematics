program Least_Common_Multiple_of_N_Integers
    ! N          : 整数列 A の長さ
    ! A          : 最小公倍数を求めたい整数列
    ! lcm_result : 計算された最小公倍数
    implicit none
    integer(16) :: N, lcm_result
    integer(16), allocatable :: A(:)

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    ! 最小公倍数の計算
    call compute_lcm_of_array(A, lcm_result)

    ! 結果の出力
    print *, lcm_result

contains
    ! 数列全体の最小公倍数を計算するサブルーチン
    subroutine compute_lcm_of_array(A, result)
        integer(16), dimension(:), intent(in) :: A
        integer(16), intent(out) :: result
        integer(16) :: i

        ! 最初の要素の値を初期値として設定
        result = A(1)

        ! 配列全体のLCMを計算
        do i = 2, size(A)
            result = lcm(result, A(i))
        end do
    end subroutine compute_lcm_of_array

    ! 2つの数の最小公倍数を計算する関数
    integer(16) function lcm(a, b)
        integer(16) :: a, b
        lcm = abs(a*b)/gcd(a, b)
    end function lcm

    ! 2つの数の最大公約数を計算する関数 (ユークリッドの互除法)
    integer(16) function gcd(a, b)
        integer(16) :: a, b, r

        ! ユークリッドの互除法を使用してGCDを計算
        do while (b /= 0)
            r = mod(a, b)
            a = b
            b = r
        end do
        gcd = a
    end function gcd

end program Least_Common_Multiple_of_N_Integers
