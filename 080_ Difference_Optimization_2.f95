program Difference_Optimization_2
    !この回答はchatGPTの力を借りて作成しています。

    ! 概要:
    ! 無向グラフ上で |x_u - x_v| <= w の制約を満たしつつ x_1=0 として
    ! x_N の最大値を求める。ただし無限に大きくできる場合は -1 を出力する。
    ! ダイクストラ法を用いて最短経路を求める。
    ! ※ヒープを使う理由:
    !   単純な線形探索ではO(M*N)かかるが、ヒープ(優先度付きキュー)を使うと
    !   O(M log N) で済み、高速に探索できる。

    ! N           : 頂点数
    ! M           : 辺の数
    ! i           : ループカウンタ
    ! u           : 現在処理中の頂点番号
    ! v           : 隣接する頂点番号
    ! w           : 重み（制約値）
    ! dist(N)     : 頂点1から各頂点までの最短距離
    ! used(N)     : 各頂点が確定済みかどうか
    ! deg(N)      : 各頂点の次数
    ! cnt(N)      : 隣接リスト格納用カウンタ
    ! adj(N,*)    : 隣接リスト
    ! heap(2,*)   : ヒープ（距離・頂点のペア）
    ! heap_size   : ヒープ内の要素数

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer(int64), parameter :: INF = 10_int64**18
    type edge
        integer(int64) :: to, cost
    end type edge
    integer(int64), allocatable :: dist(:), heap(:, :)
    type(edge), allocatable :: adj(:, :)
    integer, allocatable :: deg(:), cnt(:)
    integer(int64) :: N, M, i, u, v, w, heap_size
    logical, allocatable :: used(:)

    ! 入力
    read (*, *) N, M
    allocate (dist(N), used(N), deg(N), cnt(N))
    allocate (heap(2, M*2))
    dist = INF
    used = .false.
    deg = 0
    cnt = 0
    heap_size = 0
    do i = 1, M
        read (*, *) u, v, w
        deg(u) = deg(u) + 1
        deg(v) = deg(v) + 1
    end do
    allocate (adj(N, maxval(deg)))
    rewind (5) ! 標準入力を巻き戻して再読み込み
    read (*, *) N, M

    ! 隣接リストを構築
    do i = 1, M
        read (*, *) u, v, w
        cnt(u) = cnt(u) + 1
        adj(u, cnt(u)) = edge(v, w)
        cnt(v) = cnt(v) + 1
        adj(v, cnt(v)) = edge(u, w)
    end do

    ! 始点をヒープに入れる
    dist(1) = 0
    call push(heap, heap_size, 1_int64, 0_int64)

    ! ダイクストラ本体
    do while (heap_size > 0)
        call pop(heap, heap_size, u, w)
        if (used(u)) cycle
        used(u) = .true.

        do i = 1, deg(u)
            v = adj(u, i)%to
            w = adj(u, i)%cost
            if (dist(v) > dist(u) + w) then
                dist(v) = dist(u) + w
                call push(heap, heap_size, v, dist(v))
            end if
        end do
    end do

    ! 結果の出力
    if (dist(N) == INF) then
        print *, -1
    else
        print *, dist(N)
    end if

contains

    ! ヒープに (u, d) を挿入する
    ! 最小ヒープを構築することで、次に処理すべき頂点をO(log N)で取り出せる
    subroutine push(heap, heap_size, u, d)
        integer(int64), intent(inout) :: heap(:, :), heap_size
        integer(int64), intent(in) :: u, d
        integer(int64) :: i, parent

        heap_size = heap_size + 1
        i = heap_size
        heap(1, i) = d
        heap(2, i) = u

        ! ヒープの上にバブルアップする
        do while (i > 1)
            parent = i/2
            if (heap(1, i) >= heap(1, parent)) exit
            call swap(heap, i, parent)
            i = parent
        end do
    end subroutine push

    ! ヒープの最小要素 (u, d) を取り出す
    ! ヒープの根を取り出し、末尾の要素を根に移し、ヒープ条件を回復する
    subroutine pop(heap, heap_size, u, d)
        integer(int64), intent(inout) :: heap(:, :), heap_size
        integer(int64), intent(out) :: u, d
        integer(int64) :: i, left, right, smallest

        d = heap(1, 1)
        u = heap(2, 1)

        heap(1, 1) = heap(1, heap_size)
        heap(2, 1) = heap(2, heap_size)
        heap_size = heap_size - 1

        i = 1
        ! ヒープの下にバブルダウンする
        do
            left = i*2
            right = i*2 + 1
            smallest = i
            if (left <= heap_size .and. heap(1, left) < heap(1, smallest)) smallest = left
            if (right <= heap_size .and. heap(1, right) < heap(1, smallest)) smallest = right
            if (smallest == i) exit
            call swap(heap, i, smallest)
            i = smallest
        end do
    end subroutine pop

    ! ヒープの要素 i と j を入れ替える
    subroutine swap(heap, i, j)
        integer(int64), intent(inout) :: heap(:, :)
        integer(int64), intent(in) :: i, j
        integer(int64) :: tmp_d, tmp_u

        tmp_d = heap(1, i)
        tmp_u = heap(2, i)
        heap(1, i) = heap(1, j)
        heap(2, i) = heap(2, j)
        heap(1, j) = tmp_d
        heap(2, j) = tmp_u
    end subroutine swap

end program Difference_Optimization_2
