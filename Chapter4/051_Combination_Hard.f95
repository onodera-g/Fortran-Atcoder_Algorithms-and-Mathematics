program Combination_Hard
    ! 入力値       : x, y (目的地の座標)
    ! m           : 素数(1000000007)
    ! nfact       : (x+y)! mod m を保持する変数
    ! xfact       : x! mod m を保持する変数
    ! yfact       : y! mod m を保持する変数
    ! denominator : (x! * y!) mod m を保持する変数
    ! answer      : 組み合わせ計算結果 ((x+y)! / (x! * y!)) mod m を保持する変数
    implicit none
    integer(8) :: x, y
    integer(8), parameter :: m = 1000000007
    integer(8) :: nfact, xfact, yfact, denominator, answer

    integer(8), external :: division, fractorial

    read (*, *) x, y

    ! (x+y)! と x!, y! を計算
    nfact = fractorial(x + y, m)
    xfact = fractorial(x, m)
    yfact = fractorial(y, m)

    ! denominator = (x! * y!) mod m
    denominator = mod(xfact*yfact, m)

    ! answer = ((x+y)! / (x! * y!)) mod m
    answer = division(nfact, denominator, m)

    write (*, "(i0)") answer
end program Combination_Hard
! n! mod m を求める関数
function fractorial(n, m)
    implicit none
    integer(8), intent(in) :: n, m
    integer(8) :: fractorial
    integer(8) :: i

    fractorial = 1
    do i = 1, n
        fractorial = mod(fractorial*i, m)
    end do
end function fractorial

! a^b mod m を繰り返し二乗法で計算する関数
function powermod(a, b, m)
    implicit none
    integer(8), intent(in) :: a, b, m
    integer(8) :: powermod
    integer(8) :: base
    integer :: i

    base = a
    powermod = 1

    do i = 0, 63
        if (ibits(b, i, 1) /= 0) then
            powermod = mod(powermod*base, m)
        end if
        base = mod(base*base, m)
    end do
end function powermod

! division = (bunshi / bunbo) mod m を求める関数
! フェルマーの小定理を利用し、割り算を掛け算に変換している。
function division(bunshi, bunbo, m)
    implicit none
    integer(8), intent(in) :: bunshi, bunbo, m
    integer(8) :: division
    integer(8), external :: powermod

    division = mod(bunshi*powermod(bunbo, m - 2, m), m)
end function division
