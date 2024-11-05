program Binary__Search
    implicit none
    integer(16) N, X, ans
    integer(16) left, right
    integer(16), allocatable :: A(:)

    ! 入力読み込み
    read (*, *) N, X
    allocate (A(N))
    read (*, *) A

    ! ソートを行う（マージソート）
    call margesort(A, N)

    ! 二分探索の実行
    ans = 0
    left = 1
    right = N
    call binary_search(A, N, left, right, X, ans)

    ! 結果の出力
    if (ans /= 0) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program

! マージソート
subroutine margesort(x, n)
    integer(16) N
    integer(16) x(N), tmp(N)
    call loop_margesort(x, tmp, N, 1_16, N)
end subroutine

recursive subroutine loop_margesort(x, tmp, N, left, right)
    integer(16) left, right, mid
    integer(16) N
    integer(16) x(N), tmp(N)
    integer(16) i, j, k

    if (left >= right) return
    mid = (left + right)/2_16
    call loop_margesort(x, tmp, N, left, mid)
    call loop_margesort(x, tmp, N, mid + 1_16, right)

    ! マージ部分
    i = left
    j = mid + 1_16
    k = left
    do while (i <= mid .and. j <= right)
        if (x(i) <= x(j)) then
            tmp(k) = x(i)
            i = i + 1_16
        else
            tmp(k) = x(j)
            j = j + 1_16
        end if
        k = k + 1_16
    end do

    ! 残りの要素をコピー
    do while (i <= mid)
        tmp(k) = x(i)
        i = i + 1_16
        k = k + 1_16
    end do

    do while (j <= right)
        tmp(k) = x(j)
        j = j + 1_16
        k = k + 1_16
    end do

    ! 元の配列にマージ結果を反映
    x(left:right) = tmp(left:right)
end subroutine

! 二分探索
recursive subroutine binary_search(x, N, left, right, target_val, ans)
    integer(16) left, right, mid, N
    integer(16) x(N), target_val
    integer(16) ans

    if (left > right) return

    mid = (left + right)/2_16

    if (x(mid) == target_val) then
        ans = mid
        return
    elseif (x(mid) > target_val) then
        call binary_search(x, N, left, mid - 1_16, target_val, ans)
    else
        call binary_search(x, N, mid + 1_16, right, target_val, ans)
    end if
end subroutine
