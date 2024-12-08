program Knight
    ! X : ナイトの駒を移動させる目標座標 X。
    ! Y : ナイトの駒を移動させる目標座標 Y。
    ! card1 : (1,2) の動きをする回数。
    ! card2 : (2,1) の動きをする回数。
    ! mod_val : 計算結果を割るモジュロの値として 1000000007 を定数として定義。
    ! tempX : 一時的な X の値を保持する変数。
    ! tempY : 一時的な Y の値を保持する変数。
    ! currentFactorial : 現在の階乗の値を保持。
    ! factorialCard1 : card1! の階乗値を保持。
    ! factorialCard2 : card2! の階乗値を保持。
    ! answer : 最終的な答えを保持。
    ! numerator : (card1 + card2)! の値を保持。
    ! denominator : (card1! * card2!) の値を保持。

    implicit none
    integer(16) :: X, Y
    integer(16) :: card1, card2
    integer(16), parameter :: mod_val = 1000000007_16
    integer(16) :: tempX, tempY
    integer(16) :: i
    integer(16) :: currentFactorial, factorialCard1, factorialCard2, answer
    integer(16) :: numerator, denominator

    ! 入力を読み込む
    read (*, *) X, Y

    card1 = 0_16
    card2 = 0_16

    ! X が Y より大きい場合の処理
    if (X > Y) then
        card1 = X - Y
        tempX = X - 2_16*(X - Y)
        tempY = Y - (X - Y)
        X = tempX
        Y = tempY
    end if

    ! Y が X より大きい場合の処理
    if (Y > X) then
        card2 = Y - X
        tempY = Y - 2_16*(Y - X)
        tempX = X - (Y - X)
        X = tempX
        Y = tempY
    end if

    ! X と Y が 0 以上かつ 3 の倍数であることを確認
    if (X < 0_16 .or. mod(X, 3_16) /= 0_16 .or. Y < 0_16 .or. mod(Y, 3_16) /= 0_16) then
        print *, 0
        stop
    end if

    card1 = card1 + X/3_16 ! Xを3で割った商をcard1に加算
    card2 = card2 + Y/3_16 ! Yを3で割った商をcard2に加算

    ! (card1 + card2)! を計算しつつ、card1!, card2! も計算
    currentFactorial = 1_16
    factorialCard1 = 1_16
    factorialCard2 = 1_16
    numerator = 1_16

    do i = 1_16, card1 + card2
        currentFactorial = mod(currentFactorial*mod(i, mod_val), mod_val)
        if (i == card1) then
            factorialCard1 = currentFactorial ! card1! を保存
        end if
        if (i == card2) then
            factorialCard2 = currentFactorial ! card2! を保存
        end if
        if (i == card1 + card2) then
            numerator = currentFactorial ! (card1 + card2)! を保存
        end if
    end do

    ! 分母 (card1! * card2!) を計算
    denominator = mod(factorialCard1*factorialCard2, mod_val)

    ! モジュラー逆数を計算し、最終的な答えを求める
    answer = mod(numerator*getModularReciprocal(denominator, mod_val), mod_val)

    ! 結果を出力
    print *, answer

contains

    ! モジュラー逆数を計算する関数
    function getModularReciprocal(num, mod_num) result(answer)
        integer(16) num, mod_num ! 入力値
        integer(16) :: answer ! 逆数の結果5
        integer :: j ! ビットカウンタ

        answer = 1_16
        do j = 0, 60 ! 64ビット整数を想定
            if (btest(mod_num - 2_16, j)) then
                answer = mod(answer*num, mod_num) ! num^(2^j) を乗算
            end if
            num = mod(num*num, mod_num) ! numを2乗
        end do
    end function getModularReciprocal

end program Knight
