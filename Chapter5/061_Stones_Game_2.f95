program StonesGame2
    ! --概要--
    ! 石の数を 0 個から徐々に増やして最善手での勝敗を考えると、
    ! N+1 が 2 のべき乗（つまり log2(N+1) が整数）の場合に限り、
    ! その状態は後手必勝となる。
    ! つまり、2^x - 1 の形の N に対しては後手が勝つ（Second）、
    ! それ以外は先手が勝つ（First）ことが知られている。

    ! N       : 与えられた石の数
    ! M       : N+1 の値（2のべき乗かを調べる対象）
    ! logval  : log2(M) の値
    ! frac    : logval の小数部分
    ! EPS     : 小数誤差の許容値

    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none

    integer(int64) :: N
    integer(int64) :: M
    real(real64) :: logval
    real(real64) :: frac
    real(real64), parameter :: EPS = 1.0d-8

    ! 入力
    read (*, *) N

    ! M = N + 1
    M = N + 1

    ! log2(M) を求める（log(M)/log(2)）
    logval = log(real(M, real64))/log(2.0_real64)

    ! 小数部を取り出す
    frac = logval - floor(logval)

    ! log2(M) が整数なら M は 2 のべき乗 → Second（後手必勝）
    if (frac < EPS) then
        print *, "Second"
    else
        print *, "First"
    end if

end program StonesGame2
