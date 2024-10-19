program Answer_Exam_Randomly
    ! N   : 問題数
    ! P   : 各問題の選択肢
    ! Q   : 各問題の配転
    ! ans : 期待値
    implicit none
    integer(16) N, i
    real(16) P(200), Q(200), ans

    ! 入力
    read (*, *) N
    do i = 1, N
        read (*, *) P(i), Q(i)
    end do

    ! 期待値の計算
    ans = 0
    do i = 1, N
        ans = ans + Q(i)/P(i)
    end do

    ! 結果の出力
    write (*, *) ans

end program Answer_Exam_Randomly
