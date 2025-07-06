program Interval_Scheduling_Problem
    ! 概要:
    ! N 本の映画を開始時刻 L(i), 終了時刻 R(i) で表し、
    ! 最大で何本の映画を最初から最後まで見られるかを求める。
    ! 終了時刻でマージソートし、貪欲法で選ぶ。

    ! N        : 映画の本数
    ! i        : ループカウンタ
    ! count    : 見られる映画の本数（最終的な答え）
    ! last_end : 直前に見た映画の終了時刻
    ! L(N)     : 各映画の開始時刻
    ! R(N)     : 各映画の終了時刻
    ! tmpL(N)  : マージソート用の一時的な開始時刻配列
    ! tmpR(N)  : マージソート用の一時的な終了時刻配列

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, i, count, last_end
    integer(int32), allocatable :: L(:), R(:), tmpL(:), tmpR(:)

    ! 入力
    read (*, *) N
    allocate (L(N), R(N), tmpL(N), tmpR(N))

    do i = 1, N
        read (*, *) L(i), R(i)
    end do

    ! 終了時刻でマージソート
    call mergesort(L, R, tmpL, tmpR, 1, N)

    ! 貪欲に選ぶ
    count = 0
    last_end = 0

    do i = 1, N
        if (L(i) >= last_end) then
            count = count + 1
            last_end = R(i)
        end if
    end do

    ! 結果の出力
    print *, count

contains

    ! マージソート本体: 区間 [left, right] を R で昇順ソートする
    recursive subroutine mergesort(L, R, tmpL, tmpR, left, right)
        integer(int32), intent(inout) :: L(:), R(:), tmpL(:), tmpR(:)
        integer(int32), intent(in) :: left, right
        integer(int32) :: mid

        if (left >= right) return

        mid = (left + right)/2
        call mergesort(L, R, tmpL, tmpR, left, mid)
        call mergesort(L, R, tmpL, tmpR, mid + 1, right)
        call merge(L, R, tmpL, tmpR, left, mid, right)
    end subroutine mergesort

    ! 2つのソート済み区間をマージする
    subroutine merge(L, R, tmpL, tmpR, left, mid, right)
        integer(int32), intent(inout) :: L(:), R(:), tmpL(:), tmpR(:)
        integer(int32), intent(in) :: left, mid, right
        integer(int32) :: i, j, k

        tmpL(left:right) = L(left:right)
        tmpR(left:right) = R(left:right)

        i = left
        j = mid + 1
        k = left

        do while (i <= mid .and. j <= right)
            if (tmpR(i) <= tmpR(j)) then
                L(k) = tmpL(i)
                R(k) = tmpR(i)
                i = i + 1
            else
                L(k) = tmpL(j)
                R(k) = tmpR(j)
                j = j + 1
            end if
            k = k + 1
        end do

        do while (i <= mid)
            L(k) = tmpL(i)
            R(k) = tmpR(i)
            i = i + 1
            k = k + 1
        end do

        do while (j <= right)
            L(k) = tmpL(j)
            R(k) = tmpR(j)
            j = j + 1
            k = k + 1
        end do

    end subroutine merge
end program Interval_Scheduling_Problem
