program Cross_Sum
! -- 概要 --
! このプログラムは、H×W のマス目の各マスに整数が書かれているとき、
! 各マス (i,j) について、同じ行と同じ列にあるマス（自分自身を含む）の値の合計を求める。
! ただし、(i,j) の値は行と列両方に含まれて重複するため、1回分減らす。
! よって最終的な値は：B(i,j) = 行iの合計 + 列jの合計 - A(i,j)

! H, W        : 盤面の行数と列数（2 ≤ H, W ≤ 2000）
! A           : 各マスに書かれている整数（最大 99）を格納する配列
! row_sum     : 各行に含まれる数値の合計
! col_sum     : 各列に含まれる数値の合計
! i, j        : 入力・出力・計算用のループインデックス

    implicit none
    integer, parameter :: maxH = 2000, maxW = 2000
    integer :: H, W
    integer :: A(maxH, maxW)
    integer :: row_sum(maxH)
    integer :: col_sum(maxW)
    integer :: i, j

! 入力
    read (*, *) H, W
    do i = 1, H
        read (*, *) (A(i, j), j=1, W)
    end do

! A(i,j) を見ながら、対応する行と列の合計に加算
    row_sum(1:H) = 0
    col_sum(1:W) = 0
    do i = 1, H
        do j = 1, W
            row_sum(i) = row_sum(i) + A(i, j) ! 行iの合計を計算
            col_sum(j) = col_sum(j) + A(i, j) ! 列jの合計を計算
        end do
    end do

! 結果の出力
    do i = 1, H
        do j = 1, W
            write (*, '(I0,1X)', advance='no') row_sum(i) + col_sum(j) - A(i, j)
        end do
        write (*, *)
    end do

end program Cross_Sum
