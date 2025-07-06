program We_Used_to_Sing_a_Song_Together
! 概要:
! 家の位置 A と学校の位置 B が与えられる。
! 各小学生は別々の学校に通い、不便さ（距離の総和）が最小になるようにする。
! 方針: A, B をそれぞれソートし、対応させる。

! N         : 小学生の人数
! A(N)      : 小学生の家の位置
! B(N)      : 学校の位置
! tmp(N)    : マージソート用の作業配列
! i         : ループカウンタ
! total     : 最小不便さ（総和）

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer(int64) :: N, i
    integer(int64), allocatable :: A(:), B(:), tmp(:)
    integer(int64) :: total

! 入力
    read (*, *) N
    allocate (A(N), B(N), tmp(N))
    read (*, *) A
    read (*, *) B

! A と B をマージソートで昇順にソート
    call merge_sort(A, tmp, 1_int64, N)
    call merge_sort(B, tmp, 1_int64, N)

! 最小不便さを計算
    total = 0
    do i = 1, N
        total = total + abs(A(i) - B(i))
    end do

! 結果の出力
    print *, total

contains

! マージソート本体
    recursive subroutine merge_sort(arr, tmp, left, right)
        integer(int64), intent(inout) :: arr(:), tmp(:)
        integer(int64), intent(in) :: left, right
        integer(int64) :: mid

        if (left >= right) return

        mid = (left + right)/2
        call merge_sort(arr, tmp, left, mid)
        call merge_sort(arr, tmp, mid + 1, right)

        call merge(arr, tmp, left, mid, right)
    end subroutine merge_sort

! マージ処理
    subroutine merge(arr, tmp, left, mid, right)
        integer(int64), intent(inout) :: arr(:), tmp(:)
        integer(int64), intent(in) :: left, mid, right
        integer(int64) :: i, j, k

        i = left
        j = mid + 1
        k = left

        do while (i <= mid .and. j <= right)
            if (arr(i) <= arr(j)) then
                tmp(k) = arr(i)
                i = i + 1
            else
                tmp(k) = arr(j)
                j = j + 1
            end if
            k = k + 1
        end do

        do while (i <= mid)
            tmp(k) = arr(i)
            i = i + 1
            k = k + 1
        end do

        do while (j <= right)
            tmp(k) = arr(j)
            j = j + 1
            k = k + 1
        end do

        do i = left, right
            arr(i) = tmp(i)
        end do
    end subroutine merge

end program We_Used_to_Sing_a_Song_Together
