program Shortest_Path_1
    ! N          : グラフの頂点数
    ! M          : グラフの辺数
    ! A(:)       : 各辺の始点を格納する配列
    ! B(:)       : 各辺の終点を格納する配列
    ! degrees(:) : 各頂点の次数（接続している辺の数）
    ! adj_ptr(:) : CSR形式の隣接リストの開始ポインタ配列
    ! adj(:)     : CSR形式の隣接リストの隣接頂点配列
    ! current(:) : 隣接リスト構築時の各頂点の現在の挿入位置を追跡する配列
    ! distance(:): BFSにおける各頂点への最短距離を保持する配列

    ! この問題では、CSR形式で無向グラフを構築しています。
    implicit none
    integer :: N, M
    integer, allocatable :: A(:), B(:)
    integer, allocatable :: degrees(:), adj_ptr(:), adj(:), current(:)
    integer, allocatable :: distance(:)
    integer :: i, u, v

    ! 入力
    read (*, *) N, M
    allocate (A(M))
    allocate (B(M))
    do i = 1, M
        read (*, *) A(i), B(i)
    end do

    ! 頂点から伸びている辺の数
    allocate (degrees(N))
    degrees = 0
    do i = 1, M
        degrees(A(i)) = degrees(A(i)) + 1
        degrees(B(i)) = degrees(B(i)) + 1
    end do

    ! グラフを１次元配列にしたときの各辺の範囲を管理している。
    ! 例えば、頂点１からつながっている頂点のリストは、adj_ptr(1)からadj_ptr(2)-1 に格納されている。
    allocate (adj_ptr(N + 1))
    adj_ptr(1) = 1
    do i = 1, N
        adj_ptr(i + 1) = adj_ptr(i) + degrees(i)
    end do

    ! 隣接リストの構築
    allocate (adj(2*M), current(N))
    current = 0
    do i = 1, M
        u = A(i)
        v = B(i)
        ! 頂点 u に隣接する頂点 v を追加
        adj(adj_ptr(u) + current(u)) = v
        current(u) = current(u) + 1
        ! 頂点 v に隣接する頂点 u を追加
        adj(adj_ptr(v) + current(v)) = u
        current(v) = current(v) + 1
    end do

    ! BFS用の距離配列の割り当てと初期化
    allocate (distance(N + 1))
    distance = -1

    ! BFSの実行
    call perform_bfs(N, adj_ptr, adj, distance)

    ! 結果の出力
    do i = 1, N
        print *, distance(i)
    end do

contains

    subroutine perform_bfs(N, adj_ptr, adj, distance)
        implicit none
        integer, intent(in) :: N
        integer, intent(in) :: adj_ptr(:)
        integer, intent(in) :: adj(:)
        integer, intent(inout) :: distance(:)
        integer, allocatable :: queue(:)
        integer :: head, tail
        integer :: u, v, i

        ! キューの割り当て（頂点数Nが最大の必要サイズ）
        allocate (queue(N))

        head = 1
        tail = 1
        if (N >= 1) then
            queue(tail) = 1 ! 始点をキューに追加
            distance(1) = 0 ! 始点の距離を0に設定
        end if

        ! BFSの実行
        do while (head <= tail .and. N >= 1)
            u = queue(head)
            head = head + 1

            ! 頂点uに隣接するすべての頂点vを調べる
            do i = adj_ptr(u), adj_ptr(u + 1) - 1
                v = adj(i)
                if (distance(v) == -1) then
                    distance(v) = distance(u) + 1
                    tail = tail + 1
                    if (tail > N) then
                        print *, "キューがオーバーフローしました。"
                        stop
                    end if
                    queue(tail) = v
                end if
            end do
        end do

        deallocate (queue)
    end subroutine perform_bfs
end program Shortest_Path_1
