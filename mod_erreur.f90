Module mod_erreur
  !Fonction TER
  !On choisit LC tibia comme origine du repère
  Use mod_variables
  Implicit none


  Type quaternion
     Real(Pr) :: a,b,c,d

  end type quaternion

  Interface operator(-)
     Module Procedure soustraction_quat
  End Interface operator(-)

Contains
  Function soustraction_quat(p,q) Result(y)
    !soustraction quaternion

    Type(quaternion), Intent(In) :: p,q
    Type(quaternion) :: y

    y%a = p%a-q%a
    y%b = p%b-q%b
    y%c = p%c-q%c
    y%d = p%d-q%d
  End Function soustraction_quat

  Function norme2_quat(x) Result(d)
    !calcul la distance d'un vecteur (on prend a=0 dans le quaternion)
    Type(quaternion),Intent(In) :: x
    Real(PR) :: d

    d = Sqrt(x%b**2 +x%c**2 +x%d**2)
    
  End Function norme2_quat

  function prod_scal_quat(u,v) Result(r)
    !produit scalaire de 2 quaternons

    !déclaration variable entrée/sortie
    Type(quaternion), Intent(In) :: u,v
    Real(PR) :: r

    !instructions
    r = u%b*v%b  +  u%c*v%c  +  u%d*v%d
  end function prod_scal_quat

  function prod_vec_quat(u,v)Result(w)
    !produit scalaire de 2 quaternions

    !déclaration variable entrée/sortie
    Type(quaternion), Intent(In) :: u,v
    Type(quaternion) :: w

    !instructions

    w%b = u%c*v%d - (u%d*v%c)
    w%c = u%d*v%b - (u%b*v%d)
    w%d = u%b*v%c - (u%c*v%b)

  end function prod_vec_quat

  function inv_quat(u)Result(v)
    !inverse quaternion

    !déclaration variable entrée/sortie
    Type(quaternion), Intent(In) :: u
    Type(quaternion) :: v

    !déclaration variable
    Real(PR) :: som


    !instructions
    som = u%a**2    +    u%b**2 + u%c**2 + u%d**2
    v%a =  u%a/som
    v%b = -u%b/som
    v%c = -u%c/som
    v%d = -u%d/som
  end function inv_quat

  function prod_hamilton_quat(u,v)Result(w)
    Type(quaternion),intent(in) :: u,v
    Type(quaternion) :: w
    Type(quaternion) :: q


    !instruction
    q=prod_vec_quat(u,v)
    w%a= u%a*v%a -prod_scal_quat(u,v)
    w%b= u%a*v%b + v%a*u%b + q%b
    w%c= u%a*v%c + v%a*u%c + q%c
    w%d= u%a*v%d + v%a*u%d + q%d
    
  end function prod_hamilton_quat


  Function rotation_quat(theta, v, u) Result(vp)
    !calcul de la rotation d'un vecteur autour d'un axe u d'angle theta

    Real(PR), Intent(In) :: theta
    Type(quaternion), Intent(In) :: v
    Type(quaternion) :: vp
    Real(PR), Dimension(3), Intent(In) :: u

    Type(quaternion) :: q,qinv

    q%a=cos(theta/2)
    q%b=sin(theta/2)*u(1)
    q%c=sin(theta/2)*u(2)
    q%d=sin(theta/2)*u(3)

    !calcul de qvq⁻¹
    qinv = inv_quat(q)
    vp = prod_hamilton_quat(q,v)
    vp = prod_hamilton_quat(vp,qinv)
  End Function rotation_quat


  Function erreur(u, theta)
    ! Réalise le déplacement des points du fémur (translation + rotation) et renvoie la différence de la distance au point correspondant sur le tibia et à la distance attendue entre ces deux mêmes points
    Real(PR), Dimension(3), Parameter :: thetarot=(/1._PR, 0._PR, 0._PR/),&
         & phirot=(/0._PR, 1._PR, 0._PR/),psirot=(/0._PR, 0._PR, 1._PR/)
    Real(PR), Dimension(5), Parameter :: l=(/ 33.05_PR, 41.46_PR, 100.79_PR, 78.77_PR, 31.11_PR /)!vecteur ligaments (en mm) !ACL !PCL !MCL !LC !MC (distance attendue)
    
    Real(PR), Dimension(5), Intent(In) :: u !Contient, dans l'ordre, x,y,z,phi,psi,theta
    Real(PR), Intent(In) :: theta
    Real(PR) :: erreur
    
    Real(PR) :: x,y,z,phi,psi
    Type(quaternion) :: LCT,MCT,ACLT,PCLT,MCLT,   LCF,MCF,ACLF,PCLF,MCLF

    x = U(1) ; y = U(2) ; z = U(3) ; phi = U(4) ; psi = U(5)
    
    !Initialisation des points du tibia
    LCT%a = 0._PR
    LCT%b = 0._PR
    LCT%c = 0._PR
    LCT%d = 0._PR

    MCT%a = 0._PR
    MCT%b = Real(-15.95-10.58,Pr)
    MCT%c = Real(53.53+51.69,Pr)
    MCT%d = Real(-20.37-19.26,Pr)

    ACLT%a = 0._PR
    ACLT%b = Real(14.03-10.58,Pr)
    ACLT%c = Real(-1.21+51.69,Pr)
    ACLT%d = Real(-3.72-19.26,Pr)

    PCLT%a = 0._PR
    PCLT%b = Real(-17.24-10.58,Pr)
    PCLT%c = Real(-12.15+51.69,Pr)
    PCLT%d = Real(15.60-19.26,Pr)

    MCLT%a = 0._PR
    MCLT%b = Real(4.55-10.58,Pr)
    MCLT%c = Real(-72.62+51.69,Pr)
    MCLT%d = Real(-16.23-19.26,Pr)

    !Initialisation des 5 points du fémur pour theta=55°
    !et translation

    LCF%a = 0._PR
    LCF%b = 0._PR + x
    LCF%c = 0._PR + y
    LCF%d = 0._PR + z

    MCF%a=0._PR
    MCF%b=Real(-6.45-1.75,Pr) + x
    MCF%c=Real(-4.24+1.67,Pr) + y
    MCF%d=Real(-22.93-30.09,Pr) + z

    ACLF%a=0._PR
    ACLF%b=Real(-8.60-1.75,Pr) + x
    ACLF%c=Real(-1.43+1.67,Pr) + y
    ACLF%d=Real(7.28-30.09,Pr) + z

    PCLF%a=0._PR
    PCLF%b=Real(-3.66-1.75,Pr) + x
    PCLF%c=Real(-7.93+1.67,Pr) + y
    PCLF%d=Real(-5.94-30.09,Pr) + z

    MCLF%a=0._PR
    MCLF%b=Real(-7.25-1.75,Pr) + x
    MCLF%c=Real(-2.90+1.67,Pr) + y
    MCLF%d=Real(-38.05-30.09,Pr)  +z


    !rotation des 5 pointsdu fémur 


    LCF = rotation_quat(theta,LCF,thetarot)
    LCF = rotation_quat(psi,LCF,psirot)
    LCF = rotation_quat(phi,LCF,phirot)

    MCF = rotation_quat(theta,MCF,thetarot)
    MCF = rotation_quat(psi,MCF,psirot)
    MCF = rotation_quat(phi,MCF,phirot)

    ACLF = rotation_quat(theta,ACLF,thetarot)
    ACLF = rotation_quat(psi,ACLF,psirot)
    ACLF = rotation_quat(phi,ACLF,phirot)

    PCLF = rotation_quat(theta,PCLF,thetarot)
    PCLF = rotation_quat(psi,PCLF,psirot)
    PCLF = rotation_quat(phi,PCLF,phirot)

    MCLF = rotation_quat(theta,MCLF,thetarot)
    MCLF = rotation_quat(psi,MCLF,psirot)
    MCLF = rotation_quat(phi,MCLF,phirot)


    ! Calcul des distances entre un point du fémur et le point du tibia qui lui est associé puis soustrait la longueur du ligament associé

    erreur = norme2_quat(ACLF - ACLT) - l(1)
    erreur = erreur + norme2_quat(PCLF - PCLT) - l(2)
    erreur = erreur + norme2_quat(MCLF - MCLT) - l(3)
    erreur = erreur + norme2_quat(LCF - LCT) - l(4)
    erreur = erreur + norme2_quat(MCF - MCT) - l(5)

  End Function erreur
  
End Module mod_erreur
