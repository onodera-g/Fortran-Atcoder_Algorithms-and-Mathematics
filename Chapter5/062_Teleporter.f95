program Teleporter
    ! -- 概要 --
    ! 各町に1つずつ設置されたテレポーターをK回使ってどの町にたどり着くか求める。
    ! 巡回が必ず発生するため、巡回の開始位置と長さを求めてKをループ内に圧縮して高速に計算。

    ! N         : 町の数
    ! K         : テレポート回数
    ! A(N)      : 各町からのテレポート先
    ! visited(N): 各町を初めて訪れたステップ（訪問していない場合は0）
    ! path(N)   : ステップ順に訪れた町の記録
    ! pos       : 現在の町
    ! step      : 現在のステップ（回数）
    ! loop_start: 巡回の開始ステップ（0-based）
    ! loop_len  : 巡回の長さ（周期）

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer(int64) :: N, K
    integer(int64), allocatable :: A(:), visited(:), path(:)
    integer(int64) :: pos, step, loop_start, loop_len
    integer :: i

    ! 入力
    read (*, *) N, K
    allocate (A(N), visited(N), path(N))
    read (*, *) A

    visited = 0
    path = 0

    ! 巡回検出
    pos = 1
    step = 0
    do
        if (visited(pos) /= 0) then
            ! すでに訪れた町 => 巡回開始
            loop_start = visited(pos) - 1 ! 0-based
            loop_len = step - visited(pos) + 1
            exit
        end if

        step = step + 1
        visited(pos) = step
        path(step) = pos
        pos = A(pos)
    end do

    ! -- 出力処理 --
    if (K < loop_start) then
        ! ループに入る前に到達
        print *, path(K + 1)
    else
        ! ループ内をKに合わせて圧縮
        K = mod(K - loop_start, loop_len)
        print *, path(loop_start + K + 1)
    end if

end program Teleporter
