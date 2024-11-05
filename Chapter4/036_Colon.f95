program Colon
    ! A : 時針の長さ
    ! B : 分針の長さ
    ! H : 現在の時
    ! M : 現在の分
    ! pi : 円周率（π）
    ! alpha : 分針の角度（ラジアン）
    ! beta : 時針の角度（ラジアン）
    ! c : 2つの針の先端間の余弦を使った角度の計算結果
    ! 結果は2つの針の距離として表示されます。
    implicit none
    real(16) :: A, B, H, M, pi, alpha, beta, c

    ! π（円周率）を設定
    pi = acos(-1.0_16)

    ! 入力
    read *, A, B, H, M

    ! 分針の角度を計算
    alpha = pi*M/30.0_16

    ! 時針の角度を計算
    beta = pi*(60.0_16*H + M)/360.0_16

    ! 2つの角度の間の余弦値を計算
    c = cos(alpha)*cos(beta) + sin(alpha)*sin(beta)

    ! 2つの針の先端の距離を計算して出力
    print '(f0.10)', sqrt(A**2 + B**2 - 2.0_16*A*B*c)

end program Colon
