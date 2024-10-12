program Factorization
    ! N       : 入力された自然数
    ! i       : 素因数の候補となる数
    ! factors : Nの素因数を出力するための配列
    implicit none
    integer(16) N, i, tmp
    integer(16), allocatable :: factors(:)

    ! 入力
    read *, N

    ! 素因数分解
    allocate (factors(0))
    ! 2で割り続ける
    tmp = 2
    do while (mod(N, tmp) == 0)
        call append_factor(factors, tmp)
        N = N/tmp
    end do
    ! 奇数で割り続ける (√Nまで)
    i = 3
    do while (i*i <= N)
        do while (mod(N, i) == 0)
            call append_factor(factors, i)
            N = N/i
        end do
        i = i + tmp
    end do
    ! 残ったNが1より大きければ、それ自体が素数
    tmp = 1
    if (N > tmp) then
        call append_factor(factors, N)
    end if

    ! 結果の出力
    do i = 1, size(factors)
        if (i == size(factors)) then
            write (*, "(i0)", advance="no") factors(i)
        else
            write (*, "(i0,1x)", advance="no") factors(i)
        end if
    end do

contains
    ! 素因数を配列に追加するサブルーチン
    subroutine append_factor(factors, factor)
        integer(16), dimension(:), allocatable :: factors
        integer(16) :: factor
        integer(16), dimension(:), allocatable :: new_factors
        integer(16) :: array_size ! 'size' から 'array_size' に変更

        array_size = size(factors)
        allocate (new_factors(array_size + 1))
        new_factors(1:array_size) = factors
        new_factors(array_size + 1) = factor
        deallocate (factors)
        factors = new_factors
    end subroutine append_factor

end program Factorization
