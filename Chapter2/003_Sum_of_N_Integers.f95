program Sum_of_N_Integers
    ! N : 整数列の長さ
    ! A : 入力値
    implicit none
    integer N
    integer, allocatable::A(:)

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    ! 結果の出力
    write (*, *) sum(A)
end program
