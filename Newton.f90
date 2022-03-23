Module Newton

   Use jacobienne
   Use mod_LU

   Implicit none

   Real(PR) :: eps = 0.001


Contains

   Function Newtn(x_0) Result (x)
      Real(PR), Dimension(:), Intent(In) :: x_0
      Real(PR), Dimension(Size(x_0)) :: x

      ! Résoudre le système linéaire formé par J(x_n)(n_n+1 - x_n) = -F(x_n)
      x = solv_lu(Jac(x_0), -F(x_0))
      do while (norme2(x) > eps)
         x_0 = x_0 + x
         x = solv_lu(Jacob(x_0), -F(x_0))
      end do
   end function Newtn


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
