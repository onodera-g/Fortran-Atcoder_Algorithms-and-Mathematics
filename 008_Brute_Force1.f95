program Brute_Force_1
    ! N   : 整数
    ! S   : 整数の合計の閾値
    ! cnt : カードに書かれた整数の合計が S 以下となる個数

    implicit none
    integer i, j
    integer N, S, cnt

    ! 入力
    read (*, *) N, S

    !カードに書かれた整数の合計が S 以下となる個数
    cnt = 0
    do i = 1, N
        do j = 1, N
            if (i + j <= S) cnt = cnt + 1
        end do
    end do

    ! 結果の出力
    write (*, *) cnt
end program
