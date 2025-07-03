program Product_Max
    ! -- 概要 --
    ! このプログラムは、範囲 [a, b] および [c, d] の整数の中から
    ! x ∈ [a, b], y ∈ [c, d] を選んだときの積 x*y の最大値を求める。
    ! 候補は端点同士の組み合わせ (a, c), (a, d), (b, c), (b, d) の積のみを考慮すればよい。
    ! 計算量は O(1) で、実行時間制限2秒以内に収まる。

    ! a, b, c, d     : 入力で与えられる範囲の端点
    ! x, y          : 範囲端点の組み合わせに使う一時変数
    ! res           : x*y の最大値（最終的な出力）

    implicit none
    integer(8) :: a, b, c, d
    integer(8) :: x, y, res

    ! 入力
    read (*, *) a, b, c, d

    ! 初期値：最小値に設定
    res = -1000000000000000000_8

    ! 各端点の組み合わせで積を計算し、最大値を更新
    x = a; y = c
    if (x*y > res) res = x*y

    x = a; y = d
    if (x*y > res) res = x*y

    x = b; y = c
    if (x*y > res) res = x*y

    x = b; y = d
    if (x*y > res) res = x*y

    ! 結果の出力
    print *, res

end program Product_Max
