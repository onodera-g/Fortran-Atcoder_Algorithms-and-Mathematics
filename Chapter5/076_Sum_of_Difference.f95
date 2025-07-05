program Sum_of_Difference
    ! 概要
    ! N 個の整数 A(1..N) から全ての組み合わせ (i,j), i<j について
    ! |A(i)-A(j)| の総和を求めるプログラム。
    ! マージソートで A を昇順にソートし、
    ! ソート後に累積和を使って効率的に計算する。

    ! N        : 整数列の要素数
    ! A(N)     : 入力された整数列（途中で昇順にソートされる）
    ! tmp(N)   : マージソートの補助配列
    ! i        : ループ変数
    ! prefix   : i-1 までの累積和
    ! result   : |A(i)-A(j)| の総和
    implicit none

    integer(8) :: N, i, prefix, result
    integer(8), allocatable :: A(:), tmp(:)

    ! 入力
    read (*, *) N
    allocate (A(N), tmp(N))
    read (*, *) (A(i), i=1, N)

    ! マージソートで A を昇順に並べる
    call margesort(A, tmp, 1_8, N)

    ! 累積和を用いて、差の総和を計算
    result = 0
    prefix = 0
    do i = 1, N
        ! A(i)*(i-1) - Σ(1..i-1) A(j) を足していく
        result = result + A(i)*(i - 1) - prefix
        prefix = prefix + A(i)
    end do

    ! 結果の出力
    print *, result

contains

    ! マージソート本体
    subroutine margesort(x, tmp, left, right)
        ! 引数の説明
        ! x(:)    : ソート対象の配列
        ! tmp(:)  : 補助配列
        ! left    : ソートする範囲の左端
        ! right   : ソートする範囲の右端
        integer(8), intent(inout) :: x(:)
        integer(8), intent(inout) :: tmp(:)
        integer(8), intent(in) :: left, right
        if (left < right) then
            call loop_margesort(x, tmp, left, right)
        end if
    end subroutine margesort

    ! マージソートの再帰部分
    recursive subroutine loop_margesort(x, tmp, left, right)
        integer(8), intent(inout) :: x(:), tmp(:)
        integer(8), intent(in) :: left, right
        integer(8) :: mid, i, j, k

        ! 要素が1つなら終了
        if (left >= right) return

        ! 区間を二分
        mid = (left + right)/2
        call loop_margesort(x, tmp, left, mid)
        call loop_margesort(x, tmp, mid + 1, right)

        ! 左右をマージする
        j = 0
        ! 左側はそのままコピー
        tmp(left:mid) = x(left:mid)
        ! 右側は逆順にコピー
        do i = mid + 1, right
            tmp(i) = x(right - j)
            j = j + 1
        end do

        ! マージ処理
        i = left
        j = right
        do k = left, right
            if (tmp(i) < tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else
                x(k) = tmp(j)
                j = j - 1
            end if
        end do
    end subroutine loop_margesort
end program Sum_of_Difference
