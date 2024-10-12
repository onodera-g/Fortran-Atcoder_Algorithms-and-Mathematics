program Choose_Cards_3
    ! N: カードの枚数
    ! A: 各カードに書かれた数値の配列
    ! C: 各数値の出現回数を記録する配列
    ! ans: ペアの総数
    ! i: ループカウンタ
    implicit none
    integer(8) :: N, ans, i
    integer(8) :: C(99999) = 0
    integer(8), allocatable :: A(:)

    ! 入力の読み込み
    read *, N
    allocate (A(N))
    read *, A

    ! 各数値の出現回数をカウント
    do i = 1, N
        C(A(i)) = C(A(i)) + 1
    end do

    ! 和が100000となるペアを数える
    ans = 0
    do i = 1, 50000
        if (i == 50000) then
            ! C(i)の値が50000の場合は、(50000, 50000)のペアを特別に計算
            ans = ans + C(i)*(C(i) - 1)/2
        else
            ! iと(100000 - i)のペアの数を計算
            ans = ans + C(i)*C(100000 - i)
        end if
    end do

    ! 結果を出力
    write (*, *) ans
end program Choose_Cards_3
