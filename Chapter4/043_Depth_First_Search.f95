program connected_graph_dfs_iterative
    ! N: 頂点の数
    ! M: 辺の数
    ! A, B: 辺の端点
    ! degrees: 各頂点の次数（隣接する頂点の数）
    ! temp_A, temp_B: 一時的に辺の端点を保存する配列
    ! first: 各頂点の隣接リストの開始インデックス
    ! adjacency: 隣接リストを一次元配列で格納
    ! current: 各頂点の現在の挿入位置を追跡
    ! stack: DFS用のスタック
    ! stack_top: スタックのトップポインタ
    ! visited: 各頂点の訪問状態を管理
    ! connected: グラフが連結かどうかの判定結果
    implicit none
    integer :: N, M, A, B, stack_top
    integer :: i, j
    integer, allocatable :: degrees(:), temp_A(:), temp_B(:), first(:), adjacency(:), current(:), stack(:)
    logical, allocatable :: visited(:)
    logical :: connected

    ! 入力
    read (*, *) N, M
    allocate (degrees(N), temp_A(M), temp_B(M)); degrees = 0
    do i = 1, M
        read (*, *) A, B
        temp_A(i) = A
        temp_B(i) = B
        degrees(A) = degrees(A) + 1
        degrees(B) = degrees(B) + 1
    end do
    allocate (first(N + 1))
    first(1) = 1
    do i = 2, N + 1
        first(i) = first(i - 1) + degrees(i - 1)
    end do
    allocate (adjacency(2*M)); adjacency = 0
    allocate (current(N)); current = 0

    ! 辺を隣接リストに格納（無向グラフのため両方向に追加）
    do i = 1, M
        A = temp_A(i)
        B = temp_B(i)
        adjacency(first(A) + current(A)) = B
        current(A) = current(A) + 1
        adjacency(first(B) + current(B)) = A
        current(B) = current(B) + 1
    end do

    ! 訪問済み配列を割り当て、初期化
    allocate (visited(N))
    visited = .false.

    ! スタックを割り当て、初期化
    allocate (stack(N))
    stack_top = 0

    ! スタックに開始ノード（1）をプッシュし、訪問済みにマーク
    stack_top = stack_top + 1
    stack(stack_top) = 1
    visited(1) = .true.

    ! 反復的DFSの実行
    do while (stack_top > 0)
        ! スタックからノードをポップ
        i = stack(stack_top)
        stack_top = stack_top - 1

        ! ノードiの隣接ノードを探索
        do j = first(i), first(i + 1) - 1
            A = adjacency(j)
            if (.not. visited(A)) then
                ! 隣接ノードをスタックにプッシュし、訪問済みにマーク
                stack_top = stack_top + 1
                stack(stack_top) = A
                visited(A) = .true.
            end if
        end do
    end do

    ! グラフが連結かどうかを確認
    connected = .true.
    do i = 1, N
        if (.not. visited(i)) then
            connected = .false.
            exit
        end if
    end do

    ! 訪問済み配列を解放
    deallocate (visited)

    ! 結果の出力
    if (connected) then
        write (*, '(A)') "The graph is connected."
    else
        write (*, '(A)') "The graph is not connected."
    end if

end program connected_graph_dfs_iterative
