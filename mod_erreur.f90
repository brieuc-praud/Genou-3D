Module mod_erreur
   Use mod_variables
   Implicit none

   Type quaternion
      Real(PR) :: a,b,c,d
   End type quaternion

   Interface operator(-)
      Module Procedure soustraction_quat
   End Interface operator(-)

Contains
   Function print_quat(q) Result(str)
      ! Donne les coefficients du quaternion passé en paramètres dans str
      Type(quaternion) :: q
      Character(200) :: str

      Write(str, *) q%b, q%c, q%d

   End Function print_quat

   Function soustraction_quat(p,q) Result(y)
      !soustraction de quaternions
      Type(quaternion), Intent(In) :: p,q
      Type(quaternion) :: y

      y%a = p%a-q%a
      y%b = p%b-q%b
      y%c = p%c-q%c
      y%d = p%d-q%d
   End Function soustraction_quat

   Function norme2_quat(x) Result(d)
      !calcul la distance du quaternion à l'origine
      Type(quaternion),Intent(In) :: x
      Real(PR) :: d

      d = Sqrt(x%b**2 +x%c**2 +x%d**2)

   End Function norme2_quat

   function prod_scal_quat(u,v) Result(r)
      !produit scalaire de 2 quaternions

      Type(quaternion), Intent(In) :: u,v
      Real(PR) :: r

      r = u%b*v%b  +  u%c*v%c  +  u%d*v%d
   end function prod_scal_quat

   function prod_vec_quat(u,v) Result(w)
      !produit vectoriel de 2 quaternions

      Type(quaternion), Intent(In) :: u,v
      Type(quaternion) :: w

      w%b = u%c * v%d   -   u%d * v%c
      w%c = u%d * v%b   -   u%b * v%d
      w%d = u%b * v%c   -   u%c * v%b

   end function prod_vec_quat

   function inv_quat(u)Result(v)
      !inverse de quaternion

      Type(quaternion), Intent(In) :: u
      Type(quaternion) :: v

      Real(PR) :: som

      som = u%a**2    +    u%b**2 + u%c**2 + u%d**2
      v%a =  u%a/som
      v%b = -u%b/som
      v%c = -u%c/som
      v%d = -u%d/som
   end function inv_quat

   function prod_hamilton_quat(u,v)Result(w)
      !Produit hamiltonien sur les quaternions
      Type(quaternion),intent(in) :: u,v
      Type(quaternion) :: w
      Type(quaternion) :: q


      q = prod_vec_quat(u,v)
      w%a = u%a*v%a - prod_scal_quat(u,v)
      w%b = u%a*v%b + v%a*u%b + q%b
      w%c = u%a*v%c + v%a*u%c + q%c
      w%d = u%a*v%d + v%a*u%d + q%d

   end function prod_hamilton_quat


   Function rotation_quat(theta, v, u) Result(vp)
      !Applique la rotation d'angle theta et d'axe u au quaternion v

      Real(PR), Intent(In) :: theta
      Type(quaternion), Intent(In) :: v
      Type(quaternion) :: vp
      Real(PR), Dimension(3), Intent(In) :: u

      Type(quaternion) :: q,qinv

      q%a=cos(theta*0.5_PR)
      q%b=sin(theta*0.5_PR)*u(1)
      q%c=sin(theta*0.5_PR)*u(2)
      q%d=sin(theta*0.5_PR)*u(3)

      !calcul de qvq⁻¹
      qinv = inv_quat(q)
      vp = prod_hamilton_quat(q,v)
      vp = prod_hamilton_quat(vp,qinv)
   End Function rotation_quat


   Function erreur(U, theta)
      ! Réalise le déplacement des points du fémur (translation + rotation)
      ! et renvoie la différence de la distance au point correspondant sur le tibia et de la distance attendue entre ces deux mêmes points
      Real(PR), Dimension(5), Parameter :: l = (/ 78.77_PR, 31.11_PR, 33.05_PR, 41.46_PR, 100.79_PR /) ! vecteur ligaments (en mm) LC MC ACL PCL MCL (distance attendue)

      Real(PR), Dimension(5), Intent(In) :: U !Contient, dans l'ordre, x,y,z,phi,psi
      Real(PR), Intent(In) :: theta
      Real(PR) :: erreur

      Real(PR) :: x,y,z,phi,psi
      Type(Quaternion), Dimension(10) :: Points

      Integer :: i
      Type(quaternion) :: diff


      x = U(1) ; y = U(2) ; z = U(3) ; phi = U(4) ; psi = U(5)


      Points = calculer_positions(U, theta)

      ! Calcul des distances entre un point du fémur et le point du tibia qui lui est associé puis soustrait la longueur du ligament associé
      Do i=1, 5
         diff = Points(i+5) - Points(i) ! quaternion fémur - tibia
         erreur = erreur + (  norme2_quat(diff)  -  l(i)  )**2 ! Au carré pour la dérivabilité de la fonction (important pour BFGS)
      End Do

      ! = Pénalités =
      !En z : le fémur doit rester au-dessus du tibia
      If (z < 0._PR) Then
         erreur = erreur + z**2 ! Au carré pour la dérivabilité de la fonction (important pour BFGS)
      End If
      ! En phi : on veut l'angle modulo 2 pi
      If (Abs(phi) > pi) Then
         erreur = erreur + (Abs(phi)-pi)**2 ! Au carré pour la dérivabilité de la fonction (important pour BFGS)
      End If
      ! En psi : on veut l'angle modulo 2 pi
      If (Abs(psi) > pi) Then
         erreur = erreur + (Abs(psi)-pi)**2 ! Au carré pour la dérivabilité de la fonction (important pour BFGS)
      End If

   End Function erreur

   Function calculer_positions(u, theta) Result(Points)

      Real(PR), Dimension(5), Intent(In) :: U !Contient, dans l'ordre, x,y,z,phi,psi
      Real(PR), Intent(In) :: theta
      Type(Quaternion), Dimension(10) :: Points

      Type(quaternion) :: LCT,MCT,ACLT,PCLT,MCLT,   LCF,MCF,ACLF,PCLF,MCLF

      Real(PR) :: x,y,z,phi,psi

      x = U(1) ; y = U(2) ; z = U(3) ; phi = U(4) ; psi = U(5)

      ! Initialisation des points du tibia
      ! NON OPTIMISE, CES QUATERNIONS (TIBIA) DEVRAIENT ETRE DEFINIS EN TEMPS QUE PARAMETRES
      ! On choisit LC tibia comme origine du repère

      LCT%a = 0._PR
      LCT%b = 0._PR
      LCT%c = 0._PR
      LCT%d = 0._PR

      MCT%a = 0._PR
      MCT%b = -26.53_PR
      MCT%c = 105.22_PR
      MCT%d = -39.63_PR

      ACLT%a = 0._PR
      ACLT%b = 3.45_PR
      ACLT%c = 50.48_PR
      ACLT%d = -22.98_PR

      PCLT%a = 0._PR
      PCLT%b = -27.82_PR
      PCLT%c = 39.54
      PCLT%d = -3.66_PR

      MCLT%a = 0._PR
      MCLT%b = -6.03_PR
      MCLT%c = -20.93_PR
      MCLT%d = -35.49_PR

      !initialisation des points du fémur

      LCF%a = 0._PR
      LCF%b = 0._PR
      LCF%c = 0._PR
      LCF%d = 0._PR

      MCF%a = 0._PR
      MCF%b = -8.2_PR
      MCF%c = -2.57_PR
      MCF%d = -53.02

      ACLF%a = 0._PR
      ACLF%b = -10.35_PR
      ACLF%c = 0.24_PR
      ACLF%d = -22.81_PR

      PCLF%a = 0._PR
      PCLF%b = -5.41_PR
      PCLF%c = -6.26_PR
      PCLF%d = -36.03

      MCLF%a = 0._PR
      MCLF%b = -9._PR
      MCLF%c = -1.23_PR
      MCLF%d = -68.14_PR

      ! Translation du fémur relativement au tibia
      Call translater( LCF, MCF, ACLF, PCLF, MCLF , x, y, z )
      ! Rotation des 5 points du fémur
      Call tourner(LCF, MCF, ACLF, PCLF, MCLF, theta, phi, psi)

      ! Les 5 points du fémur en sortie de fonction
      Points = (/ LCT, MCT, ACLT, PCLT, MCLT, LCF, MCF, ACLF, PCLF, MCLF /)

   End Function calculer_positions

   Subroutine translater(q1, q2, q3, q4, q5, x, y, z)
      Real(PR), Intent(In) :: x,y,z
      Type(quaternion), Intent(InOut) :: q1, q2, q3, q4, q5

      q1%b = q1%b + x
      q1%c = q1%c + y
      q1%d = q1%d + z

      q2%b = q2%b + x
      q2%c = q2%c + y
      q2%d = q2%d + z

      q3%b = q3%b + x
      q3%c = q3%c + y
      q3%d = q3%d + z

      q4%b = q4%b + x
      q4%c = q4%c + y
      q4%d = q4%d + z

      q5%b = q5%b + x
      q5%c = q5%c + y
      q5%d = q5%d + z

   End Subroutine translater

   Subroutine tourner(q1, q2, q3, q4, q5, theta, phi, psi)
      Real(PR), Dimension(3), Parameter :: thetarot=(/1._PR, 0._PR, 0._PR/),&
      & phirot=(/0._PR, 1._PR, 0._PR/),psirot=(/0._PR, 0._PR, 1._PR/)

      Real(PR), Intent(In) :: theta, phi, psi
      Type(quaternion), Intent(InOut) :: q1, q2, q3, q4, q5

      q1 = rotation_quat(theta, q1, thetarot)
      q1 = rotation_quat(phi, q1, phirot)
      q1 = rotation_quat(psi, q1, psirot)

      q2 = rotation_quat(theta, q2, thetarot)
      q2 = rotation_quat(phi, q2, phirot)
      q2 = rotation_quat(psi, q2, psirot)

      q3 = rotation_quat(theta, q3, thetarot)
      q3 = rotation_quat(phi, q3, phirot)
      q3 = rotation_quat(psi, q3, psirot)

      q4 = rotation_quat(theta, q4, thetarot)
      q4 = rotation_quat(phi, q4, phirot)
      q4 = rotation_quat(psi, q4, psirot)

      q5 = rotation_quat(theta, q5, thetarot)
      q5 = rotation_quat(phi, q5, phirot)
      q5 = rotation_quat(psi, q5, psirot)

   End Subroutine tourner

End Module mod_erreur
