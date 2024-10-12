program Choose_Cards_1
    ! N    :  カードの枚数
    ! A    : 各カードの色
    ! ans  : 同じカードを2枚選び方法の数
    ! cntR : 赤色の個数
    ! cntY : 黄色の個数
    ! cntB : 青色の個数
    implicit none
    integer(16) N, A(500000), i
    integer(16) cntR, cntY, cntB, ans

    ! 入力
    read (*, *) N
    read (*, *) (A(i), i=1, N)

    ! 各色の出現回数のカウント
    cntR = 0; cntY = 0; cntB = 0
    do i = 1, N
        select case (A(i))
        case (1)
            cntR = cntR + 1
        case (2)
            cntY = cntY + 1
        case (3)
            cntB = cntB + 1
        end select
    end do

    ! 同じ色の組み合わせの計算
    ans = (cntR*(cntR - 1))/2 + (cntY*(cntY - 1))/2 + (cntB*(cntB - 1))/2

    ! 結果の出力
    write (*, *) ans

end program Choose_Cards_1
