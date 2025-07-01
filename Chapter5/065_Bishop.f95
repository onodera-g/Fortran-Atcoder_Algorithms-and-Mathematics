program Bishop
    ! --概要--
    ! 将棋の角行が左上 (1,1) から到達できるマスの個数を求める
    ! 特殊ケース: 高さまたは幅が1なら到達可能なマスは1
    ! 通常ケース: ceil(H * W / 2)

    ! H, W : 盤面の高さと幅
    ! result : 到達できるマスの個数

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer(int64) :: H, W, result
    integer(int64) :: total

    ! 入力
    read (*, *) H, W

    if (H == 1_int64 .or. W == 1_int64) then
        result = 1_int64
    else
        total = H*W
        result = (total + 1_int64)/2_int64 ! ← 切り上げ相当（奇数なら+1して割る）
    end if

    ! 結果の出力
    print *, result
end program Bishop
