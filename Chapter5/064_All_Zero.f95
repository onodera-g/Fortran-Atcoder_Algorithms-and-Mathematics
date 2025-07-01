program All_Zero
    ! --概要--
    ! 数列AをK回の+1/-1操作で全て0にできるか判定。
    ! 判定条件：必要操作数sum(|A_i|) <= K かつ K-sumが偶数

    ! N: 数列の長さ
    ! K: 操作回数
    ! A: 数列
    ! S: 各A(i)の絶対値の総和（0にするために必要な操作回数）
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N, K, i, S
    integer(int32), dimension(50) :: A ! 最大N=50のため固定長でOK

    ! 入力
    read (*, *) N, K
    read (*, *) (A(i), i=1, N)

    ! 必要な操作回数の合計を計算
    S = 0
    do i = 1, N
        S = S + abs(A(i))
    end do

    ! 判定
    if (S <= K .and. mod(K - S, 2) == 0) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program All_Zero
