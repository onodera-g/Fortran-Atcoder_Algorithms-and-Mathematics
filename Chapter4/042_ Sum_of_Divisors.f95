program Sum_of_Divisors
    ! N : 入力された正整数 (1 <= N <= 10,000,000)
    ! d : 現在の約数を示すループ変数
    ! M : d の倍数の数 (floor(N/d))
    ! S : 総和 S = Σ(K=1 to N) K × f(K) （f(K) は K の約数の個数）
    implicit none
    integer(16) N, d, M, S

    ! 入力
    read (*, *) N

    ! 総和の計算
    S = 0
    do d = 1, N
        M = N/d
        S = S + d*M*(M + 1)/2
    end do

    ! 結果の出力
    print *, S

end program Sum_of_Divisors
