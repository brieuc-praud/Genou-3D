Module fonction
  !Fonction TER
  !On choisit LC tibia comme origine du repère
  Use variables
  Implicit none

  
  Type quaternion
     Real(Pr) :: a,b,c,d
     
  end type quaternion

  Type(quaternion) :: LCT,MCT,ACLT,PCLT,MCLT,LCF,MCF,ACLF,PCLF,MCLF,test

!!$
!!$  !Initialisation des points du tibia
!!$  LCT%a=0._Pr
!!$  LCT%b=0._Pr
!!$  LCT%c=0._Pr
!!$  LCT%d=0._Pr
!!$
!!$  MCT%a=0._Pr
!!$  MCT%b=Real(-15.95-10.58,Pr)
!!$  MCT%c=Real(53.53+51.69,Pr)
!!$  MCT%d=Real(-20.37-19.26,Pr)
!!$
!!$  ACLT%a=0._Pr
!!$  ACLT%b=Real(14.03-10.58,Pr)
!!$  ACLT%c=Real(-1.21+51.69,Pr)
!!$  ACLT%d=Real(-3.72-19.26,Pr)
!!$
!!$  PCLT%a=0._Pr
!!$  PCLT%b=Real(-17.24-10.58,Pr)
!!$  PCLT%c=Real(-12.15+51.69,Pr)
!!$  PCLT%d=Real(15.60-19.26,Pr)
!!$
!!$  MCLT%a=0._Pr
!!$  MCLT%b=Real(4.55-10.58,Pr)
!!$  MCLT%c=Real(-72.62+51.69,Pr)
!!$  MCLT%d=Real(-16.23-19.26,Pr)
!!$
!!$  !Initialisation du fémur pour theta=55°
!!$
!!$  LCF%a=0._Pr
!!$  LCF%b=Real(0,Pr)
!!$  LCF%c=Real(0,Pr)
!!$  LCF%d=Real(0,Pr)
!!$
!!$  MCF%a=0._Pr
!!$  MCF%b=Real(-6.45-1.75,Pr)
!!$  MCF%c=Real(-4.24+1.67,Pr)
!!$  MCF%d=Real(-22.93-30.09,Pr)
!!$
!!$  ACLF%a=0._Pr
!!$  ACLF%b=Real(-8.60-1.75,Pr)
!!$  ACLF%c=Real(-1.43+1.67,Pr)
!!$  ACLF%d=Real(7.28-30.09,Pr)
!!$
!!$  PCLF%a=0._Pr
!!$  PCLF%b=Real(-3.66-1.75,Pr)
!!$  PCLF%c=Real(-7.93+1.67,Pr)
!!$  PCLF%d=Real(-5.94-30.09,Pr)
!!$
!!$  MCLF%a=0._Pr
!!$  MCLF%b=Real(-7.25-1.75,Pr)
!!$  MCLF%c=Real(-2.90+1.67,Pr)
!!$  MCLF%d=Real(-38.05-30.09,Pr)


  Contains

    function distance(x)Result(d)
      !calcul la distance d'un vecteur (on prend a=0 dans le quaternion)
      
    !déclaration variable
   Type(quaternion),Intent(in) :: x
    Real(Pr) :: d,som
    !instructions
    som=0
    som= som + x%b**2 +x%c**2 +x%d**2
    d=sqrt(som)
  end function distance

  
  function prodquat(u,v)Result(r)
    !produit scalaire de 2 quaternons

    !déclaration variable entrée/sortie
    Type(quaternion),intent(in) :: u,v
    Real(Pr) :: r

    !instructions
    r=(u%b*v%b) + (u%c*v%c) + (u%d*v%d) 
  end function prodquat



  function prodvecquat(u,v)Result(w)
 !produit scalaire de 2 quaternions

    !déclaration variable entrée/sortie
    Type(quaternion),intent(in) :: u,v
    Type(quaternion) :: w

    !instructions

    w%b=u%c*v%d - (u%d*v%c)
    w%c=u%d*v%b - (u%b*v%d)
    w%d=u%b*v%c - (u%c*v%b)
     
  end function prodvecquat

  
  function invquat(u)Result(v)
    !inverse quaternion

    !déclaration variable entrée/sortie
    Type(quaternion),intent(in) :: u
    Type(quaternion) :: v

    !déclaration variable
    Real(Pr) :: som


    !instructions
    som= u%a**2 + (u%b**2 + u%c**2 + u%d**2)
    v%a=u%a/som
    v%b=-u%b/som
    v%c=-u%c/som
    v%d=-u%d/som
    

  end function invquat


  function prodhamilton(u,v)Result(w)
    Type(quaternion),intent(in) :: u,v
    Type(quaternion) :: w
    Type(quaternion) :: q


    !instruction
    q=prodvecquat(u,v)
    w%a= u%a*v%a -prodquat(u,v)
    w%b= u%a*v%b + v%a*u%b + q%b
    w%c= u%a*v%c + v%a*u%c + q%c
    w%d= u%a*v%d + v%a*u%d + q%d
    
  end function prodhamilton

  
  function rota(theta,v,u)Result(vp)
    !calcul de la rotation d'un vecteur autour d'un axe u d'angle theta
    
    !déclaration variable entrée/sortie
    Real(Pr),intent(in) :: theta
    Type(quaternion),intent(in) :: v
    Type(quaternion) :: vp
    Real(Pr),dimension(3),intent(in) :: u

    !déclaration variable
    Type(quaternion) :: q,qinv


    !instructions
    q%a=cos(theta/2)
    q%b=sin(theta/2)*u(1)
    q%c=sin(theta/2)*u(2)
    q%d=sin(theta/2)*u(3)

    !calcul de qvq⁻¹
    qinv=invquat(q)
    vp= prodhamilton(q,v)
    vp= prodhamilton(vp,qinv)
  end function  rota


  Function f(U) Result(V)
    !Réalise la rotation d'un vecteur et renvoie le vecteur après rotation

    Real(PR), Dimension(5), Parameter :: l=(/ 33.05, 41.46, 100.79, 78.77, 31.11 /)!vecteur ligaments (en mm) !ACL !PCL !MCL !LC !MC
    Real(PR), Dimension(6), Intent(In) :: U !Contient, dans l'ordre, x,y,z,phi,psi,theta

    Real(Pr),dimension(5) :: V
    Real(Pr),dimension(3) :: thetarot,phirot,psirot
    Real(PR) :: x,y,z,phi,psi,theta

    x = U(1) ; y = U(2) ; z = U(3) ; phi = U(4) ; psi = U(5) ; theta = U(6)

    thetarot=((/1,0,0/))
    psirot=((/0,1,0/))
    phirot= ((/0,0,1/))


    !Translation des 5 points

    LCF%a=0._Pr
    LCF%b=Real(0,Pr)+x
    LCF%c=Real(0,Pr)+y
    LCF%d=Real(0,Pr)+z

    MCF%a=0._Pr
    MCF%b=Real(-6.45-1.75,Pr)+x
    MCF%c=Real(-4.24+1.67,Pr)+y
    MCF%d=Real(-22.93-30.09,Pr)+z

    ACLF%a=0._Pr
    ACLF%b=Real(-8.60-1.75,Pr)+x
    ACLF%c=Real(-1.43+1.67,Pr)+y
    ACLF%d=Real(7.28-30.09,Pr)+z

    PCLF%a=0._Pr
    PCLF%b=Real(-3.66-1.75,Pr)+x
    PCLF%c=Real(-7.93+1.67,Pr)+y
    PCLF%d=Real(-5.94-30.09,Pr)+z

    MCLF%a=0._Pr
    MCLF%b=Real(-7.25-1.75,Pr)+x
    MCLF%c=Real(-2.90+1.67,Pr)+y
    MCLF%d=Real(-38.05-30.09,Pr)+z


    !rotation des 5 points


    LCF= rota(theta,LCF,thetarot)
    LCF= rota(psi,LCF,psirot)
    LCF= rota(phi,LCF,phirot)

    MCF= rota(theta,MCF,thetarot)
    MCF= rota(psi,MCF,psirot)
    MCF= rota(phi,MCF,phirot)

    ACLF= rota(theta,ACLF,thetarot)
    ACLF= rota(psi,ACLF,psirot)
    ACLF= rota(phi,ACLF,phirot)

    PCLF= rota(theta,PCLF,thetarot)
    PCLF= rota(psi,PCLF,psirot)
    PCLF= rota(phi,PCLF,phirot)

    MCLF= rota(theta,MCLF,thetarot)
    MCLF= rota(psi,MCLF,psirot)
    MCLF= rota(phi,MCLF,phirot)


    !calcul des distances entre fémur et tibia - longueur du ligament associé

    V(1)=distance(sous(ACLF,ACLT))-l(1)
    V(2)=distance(sous(PCLF,PCLT))-l(2)
    V(3)=distance(sous(MCLF,MCLT))-l(3)
    V(4)=distance(sous(LCF,LCT))-l(4)
    V(5)=distance(sous(MCF,MCT))-l(5)

  End Function f

function sous(a,b)result(y)
  !soustraction quaternion

  Type(quaternion),intent(in) :: a,b
  Type(quaternion) :: y

  y%a=a%a-b%a
  y%b=a%b-b%b
  y%c=a%c-b%c
  y%d=a%d-b%d
end function sous
end Module fonction
