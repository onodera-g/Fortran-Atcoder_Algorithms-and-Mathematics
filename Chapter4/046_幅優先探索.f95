program main
    ! elements : キューの最大要素数を設定します。ここでは10^8と非常に大きな値が設定されています。
    ! top      : キューの先頭（次に取り出す要素）のインデックス。初期値は1。
    ! tail     : キューの末尾（次に追加する位置）のインデックス。初期値は1。
    ! len      : キュー内の現在の要素数。初期値は0。
    ! que      : キューとして使用する配列。auto型の配列で動的に確保されます。
    ! dy       : 移動方向の行方向の変位。左、右、上、下の移動に対応。
    ! dx       : 移動方向の列方向の変位。左、右、上、下の移動に対応。
    ! cn       : 現在処理中のセルの座標を保持する変数（current node）。
    ! nn       : 隣接するセルの座標を一時的に保持する変数（next node）。
    ! R        : 迷路の行数。
    ! C        : 迷路の列数。
    ! sy, sx   : スタート地点の行座標（sy）と列座標（sx）。
    ! gy, gx   : ゴール地点の行座標（gy）と列座標（gx）。
    ! dist     : 各セルへの最短手数を格納する2次元配列。初期値は-1で、まだ到達していないセルを示します。
    ! S        : 迷路の各行の状態を文字列として格納する配列。
    !            各要素S(i)はi行目の文字列を表します。
    implicit none
    type :: auto
        integer(8) :: y, x
    end type auto
    integer(8) :: elements = 10**8
    integer(8) :: top = 1, tail = 1, len = 0
    type(auto), allocatable :: que(:)
    integer(8) :: dy(4) = (/0, 0, -1, 1/)
    integer(8) :: dx(4) = (/-1, 1, 0, 0/)
    type(auto) :: cn, nn
    integer(8) :: R, C, sy, sx, gy, gx, i
    integer(8), allocatable :: dist(:, :)
    character(50) :: S(50)

    ! キューのメモリを確保
    allocate (que(elements))

    ! 入力の読み込み
    read *, R, C
    allocate (dist(R, C))
    dist = -1
    read *, sy, sx, gy, gx
    do i = 1, R
        read *, S(i)
    end do

    ! スタート地点の設定
    dist(sy, sx) = 0
    cn%y = sy
    cn%x = sx
    call push(que, cn)

    ! 幅優先探索の実行
    do while (len > 0)
        ! 現在のセルをキューから取り出す
        cn = pop(que)
        ! ゴール地点に到達したらループを抜ける
        if (cn%y == gy .and. cn%x == gx) exit
        ! 隣接する4方向を探索
        do i = 1, 4
            nn%y = cn%y + dy(i)
            nn%x = cn%x + dx(i)
            ! 移動先が迷路の範囲内であり、空きマスで未訪問の場合
            if (nn%y >= 1 .and. nn%y <= R .and. nn%x >= 1 .and. nn%x <= C .and. &
                S(nn%y) (nn%x:nn%x) == "." .and. dist(nn%y, nn%x) == -1) then
                call push(que, nn)
                dist(nn%y, nn%x) = dist(cn%y, cn%x) + 1
            end if
        end do
    end do

    ! 結果の出力
    print '(i0)', dist(gy, gx)

contains

    ! サブルーチン: キューに要素を追加
    subroutine push(L, a)
        ! pushサブルーチンの変数説明
        !
        ! L : キュー配列への参照。
        !
        ! a : キューに追加する座標（auto型）。
        implicit none
        type(auto), intent(inout) :: L(:)
        type(auto), intent(in) :: a
        L(tail) = a
        tail = tail + 1
        if (tail > elements) tail = tail - elements
        len = len + 1
    end subroutine push

    ! 関数: キューから要素を取り出す
    function pop(L) result(res)
        ! pop関数の変数説明
        !
        ! L : キュー配列への参照。
        !
        implicit none
        type(auto), intent(inout) :: L(:)
        type(auto) :: res
        res = L(top)
        top = top + 1
        if (top > elements) top = top - elements
        len = len - 1
    end function pop

end program main
