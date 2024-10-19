program Knapsack1
    ! N  : 品物の数
    ! WW : ナップサックの容量（最大重さ）
    ! w  : 各品物の重さを格納する配列
    ! v  : 各品物の価値を格納する配列
    ! DP : i番目までの品物を使って、重さがj以下の場合の最大価値を保持する2次元配列
    implicit none
    integer(16) N, WW, i, j
    integer(16), allocatable ::w(:), v(:), DP(:, :)

    ! 入力
    read (*, *) N, WW
    allocate (w(0:N), v(0:N))
    allocate (dp(0:N, 0:WW))
    read (*, *) (w(i), v(i), i=1, N)

    ! DP
    dp(0, 0) = 0
    do i = 1, N
        ! i 種類を組み合わせて合計の重さを j にすると考える。
        do j = 0, WW
            ! 新しく拾いたい W(i) は j　をオーバーしているので拾えない。
            ! 前の結果を使ってコスト j を作り出す
            if (j < w(i)) DP(i, j) = DP(i - 1, j)
            ! j > w(i) なら品物をとるor取らないの選択が発生する。
            ! 拾わずに前回までの結果でコスト j　を作るか、前回のコスト j-w(i) の結果と、今回の W(i) を足してコスト j を作るかを判定
            if (j >= w(i)) DP(i, j) = max(dp(i - 1, j), dp(i - 1, j - W(i)) + v(i))
        end do
    end do

    ! 結果の出力
    write (*, *) maxval(DP) ! N 種類を考慮して、コストWを実現したときの値

end program Knapsack1
