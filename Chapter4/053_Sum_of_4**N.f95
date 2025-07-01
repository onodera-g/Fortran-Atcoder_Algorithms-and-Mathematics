program Sum_of_4N
    implicit none
    integer(8) N, m
    m = 1000000007_8
    read (*, *) N
    print '(I0)', mod((powmod(4_8, N + 1_8, m) - 1_8)*powmod(3_8, m - 2_8, m), m)
contains
    recursive integer(8) function powmod(a, b, modulus) result(out)
        implicit none
        integer(8), intent(in) :: a, b, modulus
        integer(8) :: t

        if (b .eq. 0) then
            out = 1_8
            return
        else if (b .eq. 1) then
            out = mod(a, modulus)
            return
        else if (mod(b, 2_8) .eq. 1_8) then
            out = mod(a*powmod(a, b - 1_8, modulus), modulus)
            return
        end if
        t = powmod(a, b/2_8, modulus)
        out = mod(t*t, modulus)
    end function powmod
end program Sum_of_4N
