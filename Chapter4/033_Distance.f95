program Distance
    ! ax, ay : 点Aの座標
    ! bx, by : 点Bの座標
    ! cx, cy : 点Cの座標
    ! BAx, BAy : ベクトルBAのx, y成分（点Bから点Aへのベクトル）
    ! BCx, BCy : ベクトルBCのx, y成分（点Bから点Cへのベクトル）
    ! CAx, CAy : ベクトルCAのx, y成分（点Cから点Aへのベクトル）
    ! CBx, CBy : ベクトルCBのx, y成分（点Cから点Bへのベクトル）
    ! ans : 最短距離の結果
    ! S : 三角形ABCの面積の2倍
    ! BC_length : 線分BCの長さ
    ! p : パターンを識別する変数（点Aから線分BCまでの最近接点の種類）

    implicit none
    real(16) ax, ay, bx, by, cx, cy
    real(16) BAx, BAy, BCx, BCy, CAx, CAy, CBx, CBy, ans, S, BC_length
    integer(16) p

    ! 入力
    read (*, *) ax, ay
    read (*, *) bx, by
    read (*, *) cx, cy

    ! ベクトル計算
    BAx = ax - bx; BAy = ay - by
    BCx = cx - bx; BCy = cy - by
    CAx = ax - cx; CAy = ay - cy
    CBx = bx - cx; CBy = by - cy

    ! パターン分類
    p = 2
    if (BAx*BCx + BAy*BCy < 0) p = 1
    if (CAx*CBx + CAy*CBy < 0) p = 3

    ! 距離の計算
    ans = 0
    if (p == 1) ans = sqrt(BAx*BAx + BAy*BAy)
    if (p == 3) ans = sqrt(CAx*CAx + CAy*CAy)
    if (p == 2) then
        S = abs(BAx*CAy - BAy*CAx)
        BC_length = sqrt(BCx*BCx + BCy*BCy)
        ans = S/BC_length
    end if

    ! 結果の出力
    write (*, *) ans

end program Distance
