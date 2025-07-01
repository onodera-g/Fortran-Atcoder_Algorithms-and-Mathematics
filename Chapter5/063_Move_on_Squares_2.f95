program Move_on_Squares_2
! --概要--
! N×N グリッド上を、すべてのマスを1回ずつ訪問して出発点に戻る経路が存在するか判定。
! 条件：Nが偶数なら可能、奇数なら不可能

! N: グリッドの大きさ
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer(int64) :: N

! 入力
    read (*, *) N

! 条件判定
    if (mod(N, 2_int64) == 0) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program Move_on_Squares_2
