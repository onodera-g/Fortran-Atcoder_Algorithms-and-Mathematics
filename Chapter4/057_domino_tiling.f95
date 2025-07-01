program domino_tiling
    ! K         : マス目の行数（2 <= K <= 4）
    ! N         : マス目の列数（最大 10^18）
    ! MODULO    : 結果を取る法（1000000007）
    ! S         : 状態数（2^K）: bit列で表す各行の塗り状態
    ! T(S,S)    : 遷移行列（状態 -> 状態 の遷移）
    ! A(S,S)    : 行列累乗の結果行列（初期は単位行列）
    ! B(S,S)    : 行列積の中間計算用テンポラリ
    ! vec(S)    : 初期状態ベクトル（vec(1)=1, 他は0）
    ! result(S) : 累積結果ベクトル（最終的な答えは result(1)）

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer :: K, i, j
    integer(int64) :: N
    integer(int64), parameter :: MODULO = 1000000007_int64
    integer :: S
    integer(int64), allocatable :: T(:, :), A(:, :), B(:, :), vec(:), result(:)
    integer(int64) :: exp

    ! 入力
    read (*, *) K, N
    S = 2**K
    allocate (T(S, S), A(S, S), B(S, S), vec(S), result(S))

    ! 遷移行列 T を構築（各 cur 状態から valid な next 状態への遷移）
    T = 0_int64
    do i = 0, S - 1
        call build(K, i, 0, 0_int64, T(i + 1, :))
    end do

    ! 単位行列 A の初期化（A = I）
    A = 0_int64
    do i = 1, S
        A(i, i) = 1_int64
    end do

    ! 行列 T を N 回掛ける（A = T^N）
    exp = N
    do while (exp > 0)
        if (mod(exp, 2_int64) == 1_int64) then
            call matmul_mod(A, T, B, S, MODULO)
            A = B
        end if
        call matmul_mod(T, T, B, S, MODULO)
        T = B
        exp = exp/2
    end do

    ! 初期ベクトル：vec(1) = 1（初期状態のみ 1）
    vec = 0_int64
    vec(1) = 1_int64

    ! 結果ベクトル = A × vec
    result = 0_int64
    do i = 1, S
        do j = 1, S
            result(i) = mod(result(i) + A(i, j)*vec(j), MODULO)
        end do
    end do

    ! 完全に塗られた状態に到達する方法の数：vec(1) の位置に戻る
    print *, result(1)

contains

    ! build: 再帰的に行の塗り方を探索して、次の状態への遷移を追加
    recursive subroutine build(K, cur, pos, nxt, row)
        integer, intent(in) :: K ! 行の長さ
        integer, intent(in) :: cur ! 現在の塗り状態（bit列）
        integer, intent(in) :: pos ! 現在見ている位置
        integer(int64), intent(in) :: nxt ! 次の状態の構築中のbit列
        integer(int64), intent(inout) :: row(:) ! 遷移行列の行

        if (pos == K) then
            row(nxt + 1) = mod(row(nxt + 1) + 1_int64, MODULO)
            return
        end if

        if (btest(cur, pos)) then
            ! 既に塗られているならそのまま進む
            call build(K, cur, pos + 1, nxt, row)
        else
            ! 縦置き：現在のマスを塗って次へ
            call build(K, cur, pos + 1, ibset(nxt, pos), row)

            ! 横置き：現在とその隣のマスが未使用なら2つまとめて塗る
            if (pos + 1 < K .and. .not. btest(cur, pos + 1)) then
                call build(K, cur, pos + 2, nxt, row)
            end if
        end if
    end subroutine build

    ! matmul_mod: 行列AとBの積を取り、modをとった結果をCに代入
    subroutine matmul_mod(A, B, C, S, MODULO)
        integer, intent(in) :: S
        integer(int64), intent(in) :: A(S, S), B(S, S)
        integer(int64), intent(out) :: C(S, S)
        integer(int64), intent(in) :: MODULO
        integer :: i, j, k
        integer(int64) :: sum

        C = 0_int64
        do i = 1, S
            do j = 1, S
                sum = 0_int64
                do k = 1, S
                    sum = mod(sum + A(i, k)*B(k, j), MODULO)
                end do
                C(i, j) = sum
            end do
        end do
    end subroutine matmul_mod

end program domino_tiling
