program three_cards
    ! --概要--
    ! 3枚のカードに1~Nの数を割り当て、指定条件を満たす通り数を数える。
    ! 満たさない通り数を全探索で数え、N^3 から引いて求める。

    ! N : 1~Nまでの整数を使う
    ! K : 差の最小条件
    ! total : 全通り (N^3)
    ! bad : 条件を1つも満たさない通りの数
    ! b, w, g : 黒・白・灰カードの数

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer(int64) :: N, K, total, bad
    integer(int64) :: b, w, g
    integer(int64) :: lower, upper
    integer(int64) :: ans

    ! 入力
    read (*, *) N, K

    ! 条件を満たさないケースを数える
    total = N**3
    bad = 0_int64
    do b = 1_int64, N
        do w = max(1_int64, b - (K - 1)), min(N, b + (K - 1))
            lower = max(1_int64, max(b - (K - 1), w - (K - 1)))
            upper = min(N, min(b + (K - 1), w + (K - 1)))
            do g = lower, upper
                if (abs(b - w) < K .and. abs(b - g) < K .and. abs(w - g) < K) then
                    bad = bad + 1
                end if
            end do
        end do
    end do

    ans = total - bad

    ! 出力
    print *, ans

end program three_cards
