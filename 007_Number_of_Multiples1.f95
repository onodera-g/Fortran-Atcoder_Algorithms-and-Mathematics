program Number_of_Multiples1
    ! N,X,Y : 整数
    ! cnt   : Xの倍数または Y の倍数であるものの個数

    implicit none
    integer i
    integer N, X, Y, cnt

    ! 入力
    read (*, *) N, X, Y

    !Xの倍数または Y の倍数であるものの個数
    cnt = 0
    do i = 1, N
        if (mod(i, X) == 0 .or. mod(i, Y) == 0) then
            cnt = cnt + 1
        end if
    end do

    ! 結果の出力
    write (*, *) cnt
end program
