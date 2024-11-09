program Convenience_Store2
    ! T: コンビニの開店から閉店までの時間
    ! N: 従業員の人数
    ! L: 各従業員の出勤時間を格納する配列
    ! R: 各従業員の退勤時間を格納する配列
    ! count: 各時刻における従業員の変化を格納する配列
    implicit none
    integer(16) T, N, i
    integer(16) :: L(500000), R(500000)
    integer(16), allocatable :: count(:)

    ! 入力
    read (*, *) T
    read (*, *) N
    allocate (count(0:T))
    count = 0

    ! 各従業員の出勤時間と退勤時間の処理
    do i = 1, N
        read (*, *) L(i), R(i)
        count(L(i)) = count(L(i)) + 1 ! 出勤時刻で+1
        if (R(i) < T) count(R(i)) = count(R(i)) - 1 ! 退勤時刻で-1
    end do

    ! 累積和を用いて各時刻における従業員の数を計算
    do i = 1, T - 1
        count(i) = count(i) + count(i - 1)
    end do

    ! 結果の出力
    do i = 0, T - 1
        write (*, *) count(i)
    end do
end program Convenience_Store2
