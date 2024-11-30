program Bipartite_Graph
    ! N : 頂点数
    ! M : 辺の数
    ! adj_list : 隣接リスト
    ! color : 各頂点の色を記録 (-1: 未訪問, 0: 色1, 1: 色2)
    ! queue : BFSで使用するキュー
    ! is_bipartite : 二部グラフかどうかを判定するフラグ

    ! この問題では、リンク構造隣接リストにより無向グラフを構築しています。
    !
    ! 2部グラフとは何か？
    ! https://algo-method.com/tasks/13985Rxs
    implicit none
    integer, parameter :: maxN = 200000, maxM = 400000
    integer :: N, M, i, u, v, front, back, current
    integer, allocatable :: adj_list(:), head(:), next_edge(:), color(:), queue(:)
    logical :: is_bipartite

    ! 入力
    read (*, *) N, M
    allocate (adj_list(2*M), head(N), next_edge(2*M), color(N), queue(N))
    head = -1
    next_edge = -1
    color = -1 ! 初期状態ではすべて未訪問

    ! 隣接リストの構築
    do i = 1, M
        read (*, *) u, v
        call add_edge(u, v, adj_list, head, next_edge, i*2 - 1)
        call add_edge(v, u, adj_list, head, next_edge, i*2)
    end do

    ! 二部グラフ判定
    is_bipartite = .true.
    do i = 1, N
        if (color(i) == -1) then
            front = 1
            back = 1
            queue(back) = i
            color(i) = 0

            do while (front <= back)
                current = queue(front)
                front = front + 1

                ! 隣接リストをたどる
                v = head(current)
                do while (v /= -1)
                    if (color(adj_list(v)) == -1) then
                        color(adj_list(v)) = 1 - color(current)
                        back = back + 1
                        queue(back) = adj_list(v)
                    else if (color(adj_list(v)) == color(current)) then
                        is_bipartite = .false.
                        exit
                    end if
                    v = next_edge(v)
                end do
                if (.not. is_bipartite) exit
            end do
        end if
        if (.not. is_bipartite) exit
    end do

    ! 結果の出力
    if (is_bipartite) then
        print *, "Yes"
    else
        print *, "No"
    end if

contains

    ! 辺を隣接リストに追加するサブルーチン
    subroutine add_edge(u, v, adj_list, head, next_edge, edge_index)
        integer, intent(in) :: u, v, edge_index
        integer, intent(inout) :: adj_list(:), head(:), next_edge(:)

        adj_list(edge_index) = v
        next_edge(edge_index) = head(u)
        head(u) = edge_index
    end subroutine add_edge

end program bipartite_graph
