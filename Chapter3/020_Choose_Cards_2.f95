program Choose_Cards_2
    ! N    :  カードの枚数
    ! A    : 各カードの数
    ! ans  :合計が1000になる選び方法の数
    implicit none
    integer(16) N, A(500000), i, j, k, l, m
    integer(16) ans

    ! 入力
    read (*, *) N
    read (*, *) (A(i), i=1, N)

    ! 各色の出現回数のカウント
    ans = 0
    do i = 1, N
        do j = i + 1, N
            do k = j + 1, N
                do l = k + 1, N
                    do m = l + 1, N
                        if (A(i) + A(j) + A(k) + A(l) + A(m) == 1000) ans = ans + 1
                    end do
                end do
            end do
        end do
    end do

    ! 結果の出力
    write (*, *) ans

end program Choose_Cards_2
