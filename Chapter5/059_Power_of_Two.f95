program Power_of_Two
    ! --概要--
    ! 2^N の 一の位 は周期性を持ちます。( 一の位は [2, 4, 8, 6] の繰り返し。)

    ! N         : 指数（2 の N 乗）
    ! rem       : N を 4 で割った余り
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer(int64) :: N
    integer(int64) :: rem
    integer, dimension(4) :: pattern = [2, 4, 8, 6]

    ! 入力
    read (*, *) N

    ! N mod 4 を求める
    rem = mod(N, 4_int64)

    ! 余りが 0 の場合は4番目（index=4）を参照
    if (rem == 0) then
        print *, pattern(4)
    else
        print *, pattern(rem)
    end if
end program Power_of_Two
