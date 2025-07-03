program Linear_Programming
    ! 概要
    ! このプログラムは、与えられた N 個の線形不等式制約
    ! a(i)*x + b(i)*y <= c(i) をすべて満たす正の実数 (x, y) のうち、
    ! x + y の最大値を求める問題を解く。
    ! 解法としては、各制約の2本を選んで連立方程式を解き、
    ! 交点(x, y)がすべての制約を満たしているかを確認しながら、
    ! x + y の最大値を記録する。
    ! 制約数 N は最大500なので、O(N^3) 時間で全ての交点を調べても間に合う。

    ! N               : 制約式の数
    ! a(N), b(N), c(N): 各制約式の係数と定数項
    ! i, j, k         : ループ変数
    ! x, y            : 交点の座標
    ! sum_xy          : 各交点での x + y
    ! max_xy          : 条件を満たす中での最大の x + y
    ! ok              : 各交点がすべての制約を満たすかどうかのフラグ
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 100)
    integer :: N, i, j, k
    real(dp), dimension(500) :: a, b, c
    real(dp) :: x, y, sum_xy, max_xy
    logical :: ok

    ! 入力
    read (*, *) N
    do i = 1, N
        read (*, *) a(i), b(i), c(i)
    end do

    max_xy = -1.0d100

    ! 各制約の交点を調べる
    do i = 1, N
        do j = i + 1, N
            ! 係数行列の行列式を計算
            if (abs(a(i)*b(j) - a(j)*b(i)) < 1.0d-12) cycle ! 平行な場合はスキップ

            ! 連立方程式を解く（クラメルの公式）
            x = (c(i)*b(j) - c(j)*b(i))/(a(i)*b(j) - a(j)*b(i))
            y = (a(i)*c(j) - a(j)*c(i))/(a(i)*b(j) - a(j)*b(i))

            if (x < 0.0d0 .or. y < 0.0d0) cycle ! x, y は正の実数である必要がある

            ! この点がすべての制約を満たすか確認
            ok = .true.
            do k = 1, N
                if (a(k)*x + b(k)*y > c(k) + 1.0d-8) then
                    ok = .false.
                    exit
                end if
            end do

            if (ok) then
                sum_xy = x + y
                if (sum_xy > max_xy) max_xy = sum_xy
            end if
        end do
    end do

    ! 結果の出力
    print '(F20.10)', max_xy
end program Linear_Programming
