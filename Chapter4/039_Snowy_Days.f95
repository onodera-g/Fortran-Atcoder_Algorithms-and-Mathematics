program Snowy_Days
    ! N: 区画の数
    ! Q: 雪が降る日数
    ! L: 各日で雪が降り始める区画の左端
    ! R: 各日で雪が降る区画の右端
    ! X: 各日で積もる雪の量 (cm)
    ! sum: 各区画の最終的な積雪量の変化を格納する配列
    implicit none
    integer(16) i
    integer(16) N, Q
    integer(16) L(100000), R(100000), X(100000), sum(100001)

    ! 入力
    read (*, *) N, Q
    do i = 1, Q
        read (*, *) L(i), R(i), X(i)
    end do

    ! 各日について積雪量を更新する
    sum = 0
    do i = 1, Q
        sum(L(i)) = sum(L(i)) + X(i) ! L(i) からの積雪増加
        sum(R(i) + 1) = sum(R(i) + 1) - X(i) ! R(i) + 1 で減少させて累積効果を止める
    end do

    ! 積雪量の累積和を計算
    do i = 1, N
        if (i > 1) sum(i) = sum(i) + sum(i - 1)
    end do

    ! 結果の出力
    do i = 1, N - 1
        if (sum(i) == sum(i + 1)) then
            write (*, '(a1)', advance='no') '='
        else if (sum(i) < sum(i + 1)) then
            write (*, '(a1)', advance='no') '<'
        else
            write (*, '(a1)', advance='no') '>'
        end if
    end do

end program Snowy_Days
