program Convenience_Store_1
    ! N      : 商品の数
    ! A      : 商品の値段
    ! ans    : 500円になる組み合わせの総数
    ! cnt100 : 100円の商品の数
    ! cnt200 : 200円の商品の数
    ! cnt300 : 300円の商品の数
    ! cnt400 : 400円の商品の数
    implicit none
    integer(16) N, A(200000), i, ans
    integer(16) cnt100, cnt200, cnt300, cnt400

    ! 入力
    read (*, *) N
    read (*, *) (A(i), i=1, N)

    ! 各値段の出現回数のカウント
    cnt100 = 0; cnt200 = 0; cnt300 = 0; cnt400 = 0
    do i = 1, N

        select case (A(i))
        case (100)
            cnt100 = cnt100 + 1
        case (200)
            cnt200 = cnt200 + 1
        case (300)
            cnt300 = cnt300 + 1
        case (400)
            cnt400 = cnt400 + 1
        end select
    end do

    ! 500の組み合わせの計算
    ans = (cnt100*cnt400) + (cnt200*cnt300)

    ! 結果の出力
    write (*, *) ans

end program Convenience_Store_1
