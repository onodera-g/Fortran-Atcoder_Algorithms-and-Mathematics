program Difference_Optimization_1
    ! 概要:
    ! N人の家族のうち、人1の年齢が0歳で、
    ! 他の人も全員 年齢差が「0または1」という条件でM本の辺が与えられる。
    ! 人1からBFSで到達可能な最大年齢を決める。
    ! 人1から到達できない人は年齢120とする。

    ! N, M      : 人数と制約の数
    ! deg(N)    : 各人の隣接頂点数
    ! Gp(N+1)   : CSRのポインタ
    ! Gc(2M)    : 隣接リスト
    ! queue(N)  : BFS用キュー
    ! age(N)    : 1番の人からの距離（年齢）、到達しなければ120
    ! front,back: キューの先頭・末尾

    implicit none
    integer(8), parameter :: MAX_AGE = 120
    integer(8) :: N, M, i, u, v
    integer(8), allocatable :: deg(:)
    integer(8), allocatable :: cnt(:)
    integer(8), allocatable :: Gp(:)
    integer(8), allocatable :: Gc(:)
    integer(8), allocatable :: age(:)
    integer(8), allocatable :: queue(:)
    integer(8) :: front, back, cur, nxt

    ! 入力
    read (*, *) N, M

    ! 頂点の次数を数えるための配列を用意
    allocate (deg(N), cnt(N))
    deg = 0

    ! M本の辺の入力を読み込み、各人の隣接人数をカウント
    do i = 1, M
        read (*, *) u, v
        deg(u) = deg(u) + 1
        deg(v) = deg(v) + 1
    end do

    ! CSRのポインタを構築（Gp）
    allocate (Gp(N + 1))
    Gp(1) = 1
    do i = 1, N
        Gp(i + 1) = Gp(i) + deg(i)
    end do

    ! 隣接リスト（2M個のエッジ）を確保
    allocate (Gc(2*M))
    cnt = 0

    ! 入力を巻き戻して再び読み込み、隣接リストを構築
    rewind (5)
    read (*, *) N, M
    do i = 1, M
        read (*, *) u, v
        ! uとvをそれぞれの隣接リストに登録
        Gc(Gp(u) + cnt(u)) = v
        cnt(u) = cnt(u) + 1
        Gc(Gp(v) + cnt(v)) = u
        cnt(v) = cnt(v) + 1
    end do

    ! 年齢配列とキューを初期化
    allocate (age(N), queue(N))
    age = -1 ! 未訪問は-1
    age(1) = 0 ! 人1の年齢は0

    ! BFS
    front = 1
    back = 1
    queue(1) = 1
    do while (front <= back)
        cur = queue(front) ! キューから現在の人を取り出す
        front = front + 1
        ! 現在の人の全ての隣接する人を探索
        do i = Gp(cur), Gp(cur + 1) - 1
            nxt = Gc(i)
            if (age(nxt) == -1) then
                ! まだ訪問していなければ年齢を設定
                if (age(cur) < MAX_AGE) then
                    age(nxt) = age(cur) + 1
                else
                    age(nxt) = age(cur) ! 120歳以上にはしない
                end if
                ! キューに追加
                back = back + 1
                queue(back) = nxt
            end if
        end do
    end do

    ! 結果の出力
    do i = 1, N
        if (age(i) == -1) then
            ! 人1から到達できなかった場合
            print *, MAX_AGE
        else
            print *, age(i)
        end if
    end do

end program Difference_Optimization_1
