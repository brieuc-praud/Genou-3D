Module jacobienne

  Use fonction
  
  Implicit None

  Real(PR), Parameter :: deltax=1.e-5_PR
  
Contains
  Function Jacob(U) Result(jac)
    !la jacobienne de f au point V
    Real(PR), Dimension(6), Intent(In) :: U

    Real(PR),Dimension(5,5) :: jac
    Integer :: i

    Do i=1, 5
       jac(:,i) = dvp_f(U, i)
    End Do

  end function Jacob

  Function Dvp_f(U, n) Result(V)
    ! Dérivée partielle calculée par la méthode des différences finies d'ordre 1,
    ! calculée en V, de direction n
    Real(PR), Dimension(6), Intent(In) :: U
    Real(PR), Dimension(5) :: V
    Integer, Intent(In) :: n !entier entre 1 et 5

    Real(PR), Dimension(6) :: D

    D = 0._PR
    D(n) = deltax

    V = ( f(U + D) - f(U) ) / deltax
    
  End Function Dvp_f

!!$  Function f(a, b, c, d, e)
!!$    !la fonction f à 5 variables
!!$
!!$    Real(PR), Intent(In) :: a,b, c, d, e
!!$    Real(PR), Dimension(5) :: f
!!$    
!!$    f(1)=2._PR*a**2+2._PR*b+2._PR*c+2._PR*d+2._PR*e
!!$    f(2)=2._PR*a+2._PR*b+2._PR*c+2._PR*d+2._PR*e
!!$    f(3)=2._PR*a+2._PR*b+2._PR*c+2._PR*d+2._PR*e
!!$    f(4)=2._PR*a+2._PR*b+2._PR*c+2._PR*d+2._PR*e
!!$    f(5)=2._PR*a+2._PR*b+2._PR*c+2._PR*d+2._PR*e
!!$
!!$  End Function f

End Module jacobienne
