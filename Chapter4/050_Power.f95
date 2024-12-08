program Power
    ! a   : 基数（入力値）
    ! b   : 指数（入力値）
    ! num : 割る数（定数: 1000000007）
    ! result : a^b mod num を保持する変数
    ! base   : 現在の基数（累乗計算用）
    implicit none
    integer(8) :: a, b, result, base, num = 1000000007

    ! 入力
    read (*, *) a, b

    ! 繰り返し二乗法で計算
    result = 1
    base = mod(a, num) ! 基数を初期化
    do while (b > 0)
        if (mod(b, int(2, kind=8)) == 1) then
            result = mod(result*base, num) ! 基数を乗算
        end if
        base = mod(base*base, num) ! 基数を2乗
        b = b/2 ! 指数を半分に
    end do

    ! 結果の出力
    write (*, "(i0)") result
end program Power

