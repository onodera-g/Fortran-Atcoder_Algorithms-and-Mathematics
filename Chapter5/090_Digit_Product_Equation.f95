program Digit_Product_Equation
    ! 概要:
    !   1 ≤ m ≤ N の範囲で、各桁の積を f(m) と定義する。
    !   このとき、m - f(m) = B を満たす m の個数を数える。
    !   全探索は不可能なので、m - f(m) = B から m = B + f(m) と書き換える。
    !   f(m) は最大でも 9^11 ≒ 3.5e10 なので、f(m) の候補を生成しておく。
    !   f(m) の候補 p を全て列挙し、m = B + p として、m ≤ N かつ f(m) = p ならカウント。
    !   候補 p の生成には BFS（幅優先探索）を用い、各桁の積が重複しないように探索する。
    !   これにより、計算量を抑えて解く。

    ! N, B     : 入力された上限値と定数
    ! m        : 調べる候補の整数
    ! p        : 各桁の積
    ! cnt      : 条件を満たす m の個数
    ! maxP     : 探索する p の最大値
    ! queue    : BFSで生成した各桁積候補
    ! products : 重複しない各桁積のリスト
    ! front, rear : キューの先頭・末尾インデックス
    ! m_str    : m を文字列化したもの
    ! product  : 実際にmの各桁の積を計算した結果
    ! tmp, i   : ループ変数
    ! c        : 文字列中の1文字

    implicit none
    integer(8) :: N, B, m, p, cnt, maxP
    integer(8), allocatable :: queue(:), products(:)
    integer :: front, rear, i, tmp
    character(len=20) :: m_str
    character(1) :: c
    integer(8) :: product
    logical :: found

    ! 入力
    read (*, *) N, B
    cnt = 0
    maxP = max(0_8, N - B)

    ! キュー初期化
    allocate (queue(1000000), products(1000000))
    queue(1) = 1
    products(1) = 1
    front = 1
    rear = 1

    ! BFSで各桁積の候補を生成する
    do while (front <= rear)
        p = queue(front)
        front = front + 1

        do tmp = 1, 9
            if (p*tmp <= maxP) then
                found = .false.
                do i = 1, rear
                    if (products(i) == p*tmp) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    rear = rear + 1
                    queue(rear) = p*tmp
                    products(rear) = p*tmp
                end if
            end if
        end do
    end do

    ! 0 も候補に入れる
    rear = rear + 1
    products(rear) = 0

    ! 各候補m = B + p を調べる
    do i = 1, rear
        m = B + products(i)
        if (m > N) cycle

        ! 各桁積を計算する
        write (m_str, '(I20)') m
        m_str = adjustl(m_str)

        product = 1
        do tmp = 1, len_trim(m_str)
            c = m_str(tmp:tmp)
            if (c == '0') then
                product = 0
                exit
            else
                product = product*(ichar(c) - ichar('0'))
            end if
        end do

        if (product == products(i)) then
            cnt = cnt + 1
        end if
    end do

    ! 結果の出力
    print *, cnt

end program Digit_Product_Equation
