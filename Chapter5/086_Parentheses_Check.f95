program Parentheses_Check
    ! 概要:
    ! 与えられたカッコ列が正しいかを判定するプログラム
    ! 左カッコの数をカウントしながら走査する
    ! 途中で左カッコの数が負になれば不正
    ! 最後に左カッコの数が0であれば正しいカッコ列

    ! N       : 文字列の長さ
    ! S       : カッコ列
    ! i       : ループ用カウンタ
    ! cnt     : 左カッコのカウント
    ! c       : 1文字ずつ読み込む変数

    implicit none
    integer :: N, i, cnt
    character(len=500000) :: S
    character(1) :: c

    ! 入力
    read (*, *) N
    read (*, '(A)') S(1:N)

    ! かっこのカウント
    cnt = 0
    do i = 1, N
        c = S(i:i)
        if (c == '(') then
            cnt = cnt + 1
        else if (c == ')') then
            cnt = cnt - 1
        end if
        ! カウントが負になったら不正
        if (cnt < 0) then
            print *, "No"
            stop
        end if
    end do

    ! 結果の出力
    if (cnt == 0) then !最後に0なら正しい
        print *, "Yes"
    else
        print *, "No"
    end if

end program Parentheses_Check
