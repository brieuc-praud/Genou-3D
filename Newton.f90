Module Newton
   Use jacobienne
   Use mod_LU
   Use fonction

   Implicit none

   Real(PR) :: eps = 1.e-6


Contains

   Subroutine Newtn(U)
      Real(PR), Dimension(6), Intent(InOut) :: U !U = (x,y,z,phi,psi,theta)
      Real(PR), Dimension(5) :: diff !différence x_n+1 - x_n

      ! Résoudre le système linéaire formé par J(x_n)(x_n+1 - x_n) = -F(x_n)
      diff = solv_lu(Jacob(U), -f(U))
      Do While (norme2(diff) > eps)
         U = U + (/ diff, 0._PR /)
         diff = solv_lu(Jacob(U), -f(U))
      End Do
   End Subroutine Newtn


   Function norme2(U) Result(R)
      Real(PR), Dimension(:), Intent(In) :: U
      Real(PR) :: R
      Integer :: k

      R = 0._PR
      Do k = 1, size(U)
         R = R + U(k)**2
      End Do
      R = Sqrt(R)
   End Function norme2


end module Newton
