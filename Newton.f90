Module Newton
   Use jacobienne
   Use mod_LU
   Use fonction

   Implicit none

   Real(PR) :: eps = 0.001


Contains

   Subroutine Newtn(theta, a, b, c, d, e)
      Real(PR), Intent(InOut) :: a, b, c, d, e
      Real(PR), Dimension(5) :: diff
      Real(PR) :: theta

      ! Résoudre le système linéaire formé par J(x_n)(n_n+1 - x_n) = -F(x_n)
      diff = solv_lu(Jacob(theta, a, b, c, d, e), -f(theta, a, b, c, d, e))
      Do While (norme2(diff) > eps)
         a = a + diff(1)
         b = b + diff(2)
         c = c + diff(3)
         d = d + diff(4)
         e = e + diff(5)
         diff = solv_lu(Jacob(theta, a, b, c, d, e), -f(theta, a, b, c, d, e))
      End Do
   End Subroutine Newtn


   Function norme2(V) Result(R)
      Real(PR), dimension(:), intent(in) :: V
      Real(PR) :: R
      Integer :: k

      R = 0
      Do k = 1, size(V)
         R = R + V(k)**2
      End Do
      R = sqrt(R)
   End Function norme2


end module Newton
