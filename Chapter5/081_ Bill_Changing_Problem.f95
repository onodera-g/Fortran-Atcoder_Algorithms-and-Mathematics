program Bill_Changing_Problem
    ! 概要:
    ! 与えられた金額 N を 10000, 5000, 1000 円札で最小枚数で支払う。
    ! 大きい金額の紙幣から順に使う貪欲法で解ける。

    ! N       : 支払う金額
    ! count   : 紙幣の枚数カウンタ
    ! rest    : 残りの金額
    ! tmp     : 一時的な枚数計算用

    implicit none
    integer :: N, count, rest, tmp

    ! 入力
    read (*, *) N

    count = 0
    rest = N

    ! 10000円札
    tmp = rest/10000
    count = count + tmp
    rest = rest - tmp*10000

    ! 5000円札
    tmp = rest/5000
    count = count + tmp
    rest = rest - tmp*5000

    ! 1000円札
    tmp = rest/1000
    count = count + tmp
    rest = rest - tmp*1000

    ! 結果の出力
    print *, count

end program Bill_Changing_Problem
