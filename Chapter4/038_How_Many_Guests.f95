program How_Many_Guests
    ! N   : イベントが開催される日数 (1 ≤ N ≤ 100,000)
    ! Q   : 質問の数 (1 ≤ Q ≤ 100,000)
    ! A   : 各日ごとの来場者数 (1 ≤ A_i ≤ 10^9)
    ! L   : 質問の開始日 (1 ≤ L_i ≤ R_i ≤ N)
    ! R   : 質問の終了日 (1 ≤ L_i ≤ R_i ≤ N)
    ! sum : 累積和配列。sum(i) は1日目からi日目までの合計来場者数。
    implicit none
    integer(16) :: i
    integer(16) :: N, Q
    integer(16), dimension(100000) :: A
    integer(16), dimension(100000) :: L, R
    integer(16), dimension(0:100000) :: sum

    ! 入力
    read (*, *) N, Q
    read (*, *) (A(i), i=1, N)
    do i = 1, Q
        read (*, *) L(i), R(i)
    end do

    ! 累積和の計算
    sum(0) = 0
    do i = 1, N
        sum(i) = sum(i - 1) + A(i)
    end do

    ! 質問への回答と出力
    do i = 1, Q
        if (L(i) > 1) then
            write (*, *) sum(R(i)) - sum(L(i) - 1)
        else
            write (*, *) sum(R(i))
        end if
    end do

end program How_Many_Guests
