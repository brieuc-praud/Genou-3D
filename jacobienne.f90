Module jacobienne

  Use fonction
  
  Implicit None

  Real(PR), Parameter :: delthax=0.0001
  
Contains
  Function jacob(a,b,c,d,e) Result(jac)
    !la jacobienne de f aux point (a,b,c,d,e)

    real(PR),intent(in) :: a,b,c,d,e
    real(PR),dimension(5,5) :: jac


    jac(1,:) = dvpa(a,b,c,d,e)
    jac(2,:) = dvpb(a,b,c,d,e)
    jac(3,:) = dvpc(a,b,c,d,e)
    jac(4,:) = dvpd(a,b,c,d,e)
    jac(5,:) = dvpe(a,b,c,d,e)

  end function jacob

  function dvpa(a,b,c,d,e)
    !dérivée partielle de la fonction f par rapport à la 1 ere variable

    real(PR), Intent(In) :: a,b,c,d,e
    real(PR),dimension(5) :: dvpa
    
    dvpa=(f(a+delthax,b,c,d,e)-f(a,b,c,d,e))/delthax

  end function  dvpa

  function  dvpb(a,b,c,d,e)
    !dérivée partielle de la fonction f par rapport à la 2 ere variable

    real(PR), Intent(In) :: a,b,c,d,e
    real(PR),dimension(5) :: dvpb

    dvpb=(f(a,b+delthax,c,d,e)-f(a,b,c,d,e))/delthax

  end function  dvpb

  function  dvpc(a,b,c,d,e)
    !dérivée partielle de la fonction f par rapport à la 3 ere variable

    real(PR), Intent(In) :: a,b,c,d,e
    real(PR),dimension(5) :: dvpc
    dvpc=(f(a,b,c+delthax,d,e)-f(a,b,c,d,e))/delthax

  end function dvpc

  function dvpd(a,b,c,d,e)
    !dérivée partielle de la fonction f par rapport à la 4 ere variable

    real(PR), Intent(In) :: a,b,c,d,e
    real(PR),dimension(5) :: dvpd

    dvpd=(f(a,b,c,d+delthax,e)-f(a,b,c,d,e))/delthax

  end function  dvpd

  function dvpe(a,b,c,d,e)
    !dérivée partielle de la fonction f par rapport à la 5 ere variable

    real(PR), Intent(In) :: a,b,c,d,e
    real(PR),dimension(5) :: dvpe

    dvpe=(f(a,b,c,d,e+delthax)-f(a,b,c,d,e))/delthax

  end function dvpe

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
