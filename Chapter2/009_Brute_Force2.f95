program Brute_Force2
    ! N: カードの枚数
    ! S: 目標の合計
    ! A: 各カードの値を格納する配列
    ! subset: 部分集合を表すビット列
    ! sum: 部分集合の合計を計算するための変数
    ! found: 条件を満たす部分集合が見つかったかどうかを記録する論理変数
    implicit none
    integer N, S
    integer, allocatable :: A(:)
    integer i, subset, sum
    logical found

    ! 入力
    read (*, *) N, S
    allocate (A(N))
    read (*, *) (A(i), i=1, N)

    ! すべての部分集合を試す
    found = .false.
    do subset = 0, 2**N - 1
        sum = 0
        ! 各ビットをチェックして、そのカードを選ぶかどうか決定
        do i = 1, N
            if (btest(subset, i - 1)) then
                sum = sum + A(i)
            end if
        end do
        ! 合計がSと一致するか確認
        if (sum == S) then
            found = .true.
            exit
        end if
    end do

    ! 結果の出力
    if (found) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program
