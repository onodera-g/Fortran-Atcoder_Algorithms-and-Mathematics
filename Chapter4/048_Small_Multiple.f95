program Small_Multiple
    ! QUEUE_SIZE : デックの最大サイズ
    ! K          : 入力として与えられる整数、2以上10^5以下の値
    ! visited    : 各状態での最小の桁の和を記録する配列
    ! deque_p    : デックに格納される現在の桁の和の値
    ! deque_x    : デックに格納される現在の余りの値
    ! front      : デックの先頭位置
    ! rear       : デックの末尾位置
    ! p          : 現在の桁の和
    ! x          : 現在の余り
    ! np         : 次の状態の桁の和
    ! nx         : 次の状態の余り
    implicit none
    integer, parameter :: QUEUE_SIZE = 2000000 ! デックの最大サイズ
    integer :: K, i
    integer, allocatable :: visited(:), deque_p(:), deque_x(:)
    integer :: front, rear, p, x, np, nx

    ! 入力
    read (*, *) K
    allocate (visited(0:K - 1))
    visited = huge(1)
    visited(1) = 1
    allocate (deque_p(0:QUEUE_SIZE - 1))
    allocate (deque_x(0:QUEUE_SIZE - 1))
    front = 0
    rear = 0

    ! BFSループ
    deque_p(rear) = 1
    deque_x(rear) = 1
    rear = rear + 1
    do while (front < rear)
        ! デックの先頭から現在の値を取り出す
        p = deque_p(front)
        x = deque_x(front)
        front = front + 1

        if (p > visited(x)) cycle

        ! 操作1: v = (x * 10) mod K
        np = p
        nx = mod(x*10, K)
        if (np < visited(nx)) then
            visited(nx) = np
            ! appendleft: 前に追加
            front = front - 1
            deque_p(front) = np
            deque_x(front) = nx
        end if

        ! 操作2: v = (x + 1) mod K
        np = p + 1
        nx = mod(x + 1, K)
        if (np < visited(nx)) then
            visited(nx) = np
            ! append: 後に追加
            deque_p(rear) = np
            deque_x(rear) = nx
            rear = rear + 1
        end if
    end do

    ! 結果の出力
    print *, visited(0)
end program Small_Multiple
