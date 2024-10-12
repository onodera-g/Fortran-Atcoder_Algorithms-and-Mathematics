program Calculate_GCD
    ! nums   : 最大公約数を求めたい整数 A,B
    ! result : A,B の最大公約数
    implicit none
    integer(16) nums(2), result

    ! 入力
    read (*, *) nums(1), nums(2)

    ! 最大公約数の計算
    call compute_gcd(nums, result)

    ! 結果の出力
    write (*, *) result
contains
    ! 最大公約数を計算するサブルーチン
    subroutine compute_gcd(nums, result)
        integer(16), dimension(:), intent(in) :: nums
        integer(16), intent(out) :: result
        integer(16) :: i

        ! 初期値として最初の要素のGCDを設定
        result = nums(1)

        ! 全ての数のGCDを計算
        do i = 2, size(nums)
            result = gcd(result, nums(i))
        end do
    end subroutine compute_gcd

! 2つの数のGCDを計算する関数
    integer function gcd(a, b)
        integer(16):: a, b, r
        ! ユークリッドの互除法を使用してGCDを計算
        do while (b /= 0)
            r = mod(a, b)
            a = b
            b = r
        end do
        gcd = a
    end function gcd

end program Calculate_GCD
