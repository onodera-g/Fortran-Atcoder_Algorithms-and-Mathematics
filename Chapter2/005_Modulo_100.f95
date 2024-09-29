program Modulo_100
    ! N : 整数列の長さ
    ! a : 整数列
    implicit none
    integer N
    integer ,allocatable::a(:)

    ! 入力
    read (*, *) N
    allocate(a(N))
    read(*,*)a

    ! 結果の出力
    write (*, *) mod(sum(a),100)
end program
