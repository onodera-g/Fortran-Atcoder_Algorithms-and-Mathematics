program Recurrence_Formula_2
    ! N         : 求める項番号（第N項）
    ! MODULO    : 10^9+7 での剰余を求めるための定数
    ! base(3,3) : トリボナッチ数列の漸化式を行列表現したもの
    ! res(3,3)  : 累乗の結果を格納する行列（初期は単位行列）
    ! temp(3,3) : 一時的な行列計算結果の保存用
    ! vec(3)    : 初期ベクトル [a3, a2, a1] = [2, 1, 1]
    ! ans(3)    : 結果のベクトル [a_N, a_{N-1}, a_{N-2}]
    ! exp       : 累乗の指数（N-3）
    ! rem       : 指数の2での剰余（奇数判定用）
    ! i, j      : ループ変数

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer(int64) :: N
    integer(int64), parameter :: MODULO = 1000000007_int64
    integer(int64) :: base(3, 3), res(3, 3), temp(3, 3)
    integer(int64) :: vec(3), ans(3)
    integer(int64) :: exp, rem
    integer :: i, j

    ! 入力
    read (*, *) N

    ! 初期条件の処理（Nが1〜3のときは定数を直接出力）
    if (N == 1_int64 .or. N == 2_int64) then
        print *, 1
        stop
    else if (N == 3_int64) then
        print *, 2
        stop
    end if

    ! トリボナッチ用の遷移行列を列優先で初期化
    base = reshape([1_int64, 1_int64, 0_int64, &
                    1_int64, 0_int64, 1_int64, &
                    1_int64, 0_int64, 0_int64], [3, 3])

    ! 単位行列で res を初期化
    res = 0_int64
    do i = 1, 3
        res(i, i) = 1_int64
    end do

    ! base^(N-3) を繰り返し二乗法で計算
    exp = N - 3
    do while (exp > 0)
        rem = mod(exp, 2_int64)
        if (rem == 1_int64) then
            call matmul_mod(res, base, temp, MODULO)
            res = temp
        end if
        call matmul_mod(base, base, temp, MODULO)
        base = temp
        exp = exp/2
    end do

    ! 初期ベクトル [a3, a2, a1]
    vec = [2_int64, 1_int64, 1_int64]
    ans = [0_int64, 0_int64, 0_int64]

    ! 行列 res とベクトル vec の積を計算
    do i = 1, 3
        do j = 1, 3
            ans(i) = mod(ans(i) + res(i, j)*vec(j), MODULO)
        end do
    end do

    ! 結果の出力
    print *, ans(1)

contains

    ! 3×3 行列の積を計算し、MODULO で剰余をとる
    subroutine matmul_mod(A, B, C, modulo)
        use, intrinsic :: iso_fortran_env, only: int64
        integer(int64), intent(in) :: A(3, 3), B(3, 3)
        integer(int64), intent(out) :: C(3, 3)
        integer(int64), intent(in) :: modulo
        integer(int64) :: t
        integer :: i, j, l

        C = 0_int64
        do i = 1, 3
            do j = 1, 3
                do l = 1, 3
                    t = mod(A(i, l)*B(l, j), modulo)
                    C(i, j) = mod(C(i, j) + t, modulo)
                end do
            end do
        end do
    end subroutine matmul_mod

end program Recurrence_Formula_2
