program Fibonacci_Easy
    ! n   : フィボナッチ数列の項番号（入力値）
    ! N0  : F(n-2) を保持する変数（前々項）
    ! N1  : F(n-1) を保持する変数（前項）
    ! ans : F(n) を保持する変数（現在の項）
    ! num : 割る数（定数: 1000000007）
    implicit none
    integer(16) :: n, N0 = 1, N1 = 1, ans, i, num = 1000000007

    ! 入力
    READ (*, *) n

    ! フィボナッチ数列を計算
    do i = 3, n
        ans = MOD(N0 + N1, num) ! 現在のフィボナッチ数を計算（mod num）
        N1 = N0 ! 前項を更新
        N0 = ans ! 前々項を更新
    end do

    ! 結果の出力
    write (*, "(i0)") ans
end program Fibonacci_Easy
