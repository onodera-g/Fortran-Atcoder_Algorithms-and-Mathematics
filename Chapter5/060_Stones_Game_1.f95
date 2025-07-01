program Stones_Game_1
    ! --概要--
    ! 石の数を0個から徐々に増やして最善手での勝敗を考えると、4の倍数を相手に押し付けられた方の勝ちになる。

    ! N     : 石の個数（1 以上 10^12 以下の整数）
    ! rem   : N を 4 で割った余りを格納する変数
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer(int64) :: N
    integer(int64) :: rem

    ! 入力
    read (*, *) N

    ! N を 4 で割った余りを求める
    rem = mod(N, 4_int64)

    ! 条件に応じて勝者を出力
    if (rem == 0_int64) then
        print *, "Second"
    else
        print *, "First"
    end if
end program Stones_Game_1
