program Brute_Force_2
    ! N   : 整数列の長さ
    ! S   : 整数の合計の閾値
    ! A   :
    ! cnt : カードに書かれた整数の合計が S 以下となる個数

    implicit none
    integer i, j
    integer N, S, cnt
    integer, allocatable ::A(:)

    ! 入力
    read (*, *) N, S
    allocate (A(N))
    read (*, *) A

    !カードに書かれた整数の合計が S 以下となる個数
    cnt = 0

    ! 結果の出力
    write (*, *) cnt
end program
