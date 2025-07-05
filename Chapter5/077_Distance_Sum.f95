program Distance_Sum
    ! 概要
    ! 2次元平面上の N 個の点の全ペア (i<j) について
    ! マンハッタン距離 dist(i,j) = |x_i-x_j|+|y_i-y_j| の総和を求める。
    ! ソート＋累積和で O(N log N) で計算する。

    implicit none
    integer(8) :: N, i
    integer(8), allocatable :: X(:), Y(:), tmp(:)
    integer(8) :: result, prefix

    ! 入力
    read (*, *) N
    allocate (X(N), Y(N), tmp(N))
    do i = 1, N
        read (*, *) X(i), Y(i)
    end do

    ! x座標の寄与を計算
    call merge_sort(X, tmp, 1_8, N)
    result = 0
    prefix = 0
    do i = 1, N
        result = result + X(i)*(i - 1) - prefix
        prefix = prefix + X(i)
    end do

    ! y座標の寄与を計算
    call merge_sort(Y, tmp, 1_8, N)
    prefix = 0
    do i = 1, N
        result = result + Y(i)*(i - 1) - prefix
        prefix = prefix + Y(i)
    end do

    ! 結果の出力
    print *, result

contains

    ! マージソート (再帰可能にする)
    recursive subroutine merge_sort(arr, tmp, left, right)
        integer(8), intent(inout) :: arr(:), tmp(:)
        integer(8), intent(in) :: left, right
        integer(8) :: mid
        if (left >= right) return
        mid = (left + right)/2
        call merge_sort(arr, tmp, left, mid)
        call merge_sort(arr, tmp, mid + 1, right)
        call merge(arr, tmp, left, mid, right)
    end subroutine merge_sort

    subroutine merge(arr, tmp, left, mid, right)
        integer(8), intent(inout) :: arr(:), tmp(:)
        integer(8), intent(in) :: left, mid, right
        integer(8) :: i, j, k
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
        arr(left:right) = tmp(left:right)
    end subroutine merge

end program Distance_Sum
