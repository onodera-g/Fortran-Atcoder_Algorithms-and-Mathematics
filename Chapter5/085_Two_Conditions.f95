program Two_Conditions
    ! 概要:
    ! 1以上N以下の整数の組 (a,b,c,d) で a+b+c+d=X かつ abcd=Y となるものがあるか判定する。
    ! Nは最大300なので、3重ループでa,b,cを決め、残りのdを計算して判定する。

    ! N : 上限
    ! X : 和の目標
    ! Y : 積の目標
    ! a,b,c,d : 探索する整数
    ! sum : 現在の和
    ! prod : 現在の積

    implicit none
    integer :: N
    integer(8) :: X, Y
    integer :: a, b, c, d
    integer(8) ::  prod

    ! 入力
    read (*, *) N, X, Y

    ! 探索
    do a = 1, N
        do b = 1, N
            do c = 1, N
                d = X - (a + b + c)
                if (d < 1 .or. d > N) cycle
                prod = int(a, 8)*b*c*d
                if (prod == Y) then
                    print *, "Yes"
                    stop
                end if
            end do
        end do
    end do

    ! 見つからなかった場合
    print *, "No"

end program Two_Conditions
