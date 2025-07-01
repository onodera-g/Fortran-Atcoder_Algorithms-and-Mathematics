program Recurrence1
    ! modv : 余りを取る法 (1000000007)
    ! N    : 求めたい項番号
    ! ans  : 出力用 a_N mod modv
    ! M,P  : 2×2 行列 (変換行列とその累乗結果)

    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    integer(int64), parameter :: modv = 1000000007_int64
    integer(int64) :: N
    integer(int64) :: ans
    integer(int64) :: M(2, 2), P(2, 2)

    ! 入力
    read (*, *) N

    ! ベースケース
    if (N == 1_int64 .or. N == 2_int64) then
        write (*, '(I0)') 1
        stop
    end if

    ! 変換行列 M の設定
    M = reshape([2_int64, 1_int64, 1_int64, 0_int64], [2, 2])

    ! M^(N-2) を計算
    call matpow2(M, N - 2_int64, modv, P)

    ! [a_N, a_{N-1}]^T = P * [1,1]^T なので
    ans = mod(P(1, 1) + P(1, 2), modv)

    ! 結果の出力
    write (*, '(I0)') ans

contains

    ! 2×2 行列の掛け算 (mod を取る)
    subroutine matmul2(A, B, modv, C)
        integer(int64), intent(in)  :: A(2, 2), B(2, 2)
        integer(int64), intent(in)  :: modv
        integer(int64), intent(out) :: C(2, 2)
        integer :: i, j, k

        C = 0_int64
        do i = 1, 2
            do j = 1, 2
                do k = 1, 2
                    C(i, j) = mod(C(i, j) + A(i, k)*B(k, j), modv)
                end do
            end do
        end do
    end subroutine matmul2

    ! 2×2 行列 X の n 乗を二分累乗で計算して R に格納
    subroutine matpow2(X, n, modv, R)
        integer(int64), intent(in)  :: X(2, 2), n, modv
        integer(int64), intent(out) :: R(2, 2)
        integer(int64) :: Y(2, 2), exp

        ! 単位行列に初期化
        R = 0_int64
        R(1, 1) = 1_int64
        R(2, 2) = 1_int64

        Y = X
        exp = n

        do while (exp > 0_int64)
            if (iand(exp, 1_int64) /= 0_int64) then
                call matmul2(R, Y, modv, R)
            end if
            call matmul2(Y, Y, modv, Y)
            exp = exp/2_int64
        end do
    end subroutine matpow2

end program Recurrence1
