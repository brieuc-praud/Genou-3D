Module mod_BFGS
   Use mod_LU
   Use mod_erreur

   Implicit none

   Real(PR) :: eps = 1._PR !Seuil pour la norme du gradian

 Contains
   Subroutine BFGS(u, theta)
     ! Broyden-Fletcher-Goldfarb-Shanno (recherche de 0)

     Real(PR), Dimension(5), Intent(InOut) :: u !u = (x,y,z,phi,psi)
     Real(PR), Intent(In) :: theta
     
     Real(PR), Dimension(5) :: grad, p, y ! p : direction de descente
     Real(PR), Dimension(5,5) :: B
     Real(PR), Dimension(5) :: s, Bs
     Real(PR) :: k,l,m,n,   alpha
     Integer :: i

     !Définition de la matrice identité
     B = 0._PR
     Do i=1, 5
        B(i,i) = 1._PR
     End Do

     grad = gradian(u, theta)   
     Do While ( norme2(grad) > eps)
        !!$ Print *, "norme gradian=", norme2(grad)
        ! Trouve la direction de descente p
        p = reslu(B, -grad)
        ! Trouve le pas "optimal" dans la direction de descente
        alpha = Wolfe(u, p, grad, theta)
        ! Met à jour le point
        s = alpha*p
        u = u + s
        ! Calcul de y_k
        y = gradian(u, theta) - grad
        ! Calcul de B_k+1
        k = 0._PR
        l = 0._PR
        m = 0._PR
        n = 0._PR
        Bs = Matmul(B, s)
        Do i=1, 5
           k = k + y(i)**2
           l = l + y(i)*s(i)
           m = m + Bs(i)**2
           n = n + s(i)*Bs(i)
        End Do
        B = B + k/l - m/n
        grad = y + grad !On évite ici de refaire appel à la fonction gradian()
     End Do
   End Subroutine BFGS

   Function Wolfe(x, d, grad, theta) Result(alpha)
     Real(PR), Parameter :: w1=0.1_PR, w2=0.9_PR

     Real(PR), Dimension(5), Intent(In) :: x, d, grad !point courant, direction de descente, gradian en x
     Real(PR), Intent(In) :: theta

     Real(PR), Dimension(5) :: gradad

     Real(PR) :: sup, inf,   gd, gadd,      alpha
     Integer :: i
     Logical :: critere_de_Wolfe

     critere_de_Wolfe = .False.
     inf = 0._PR
     sup = Huge(1._PR) !arbitrairement grand

     alpha = 5e2_PR
     ! La fonction f doit décroître de manière significative :
     gd = 0._PR
     Do i=1, 5
        gd = gd + grad(i)*d(i)
     End Do

     Do While (.NOT. critere_de_Wolfe )
        If ( erreur(x + alpha*d, theta) > erreur(x, theta) + w1*alpha*gd ) Then
           sup = alpha
           alpha = 0.5_PR * (inf + sup)
        Else
           gradad = gradian(x + alpha*d, theta)
           gadd = 0._PR
           Do i=1, 5
              gadd = gradad(i)*d(i)
           End Do
           ! Le pas alpha doit être suffisamment grand :
           If ( gadd < w2*gd ) Then
              inf = alpha
              If (alpha == Huge(1._PR)) Then
                 alpha = 2._PR * inf
              Else
                 alpha = 0.5_PR * (inf + sup)
              End If
           Else
              critere_de_Wolfe = .True.
           End If
        End If
     End Do
   End Function Wolfe

    Function gradian(x, theta)
      Real(PR), Dimension(5), Intent(In) :: x
      Real(PR), Intent(In) :: theta
      Real(PR), Dimension(5) :: gradian

      Integer :: i

      Do i=1, 5
         gradian(i) = dvp(x, i, theta)
      End Do
    End Function gradian

    Function dvp(x, n, theta)
      ! Dérivée partielle de f calculée par la méthode des différences finies d'ordre 1,
      ! calculée en x, de direction n
      Real(PR), Parameter :: deltax=1e-6_PR
      
      Real(PR), Dimension(5), Intent(In) :: x
      Real(PR), Intent(In) :: theta
      Integer, Intent(In) :: n !entier entre 1 et 5
      
      Real(PR) :: dvp

      Real(PR), Dimension(5) :: h

      h = 0._PR
      h(n) = deltax

      dvp = ( erreur(x + h, theta) - erreur(x, theta) ) / deltax ! Ordre 1
      !dvp = ( erreur(x-2*h, theta) - 8*erreur(x-h, theta) + 8*erreur(x+h, theta) - erreur(x+2*h, theta) ) / (12*deltax) ! Ordre 4
      
    End Function dvp

    Function norme2(U) Result(R)
      ! Norme euclidienne
      Real(PR), Dimension(:), Intent(In) :: U
      Real(PR) :: R
      Integer :: k

      R = 0._PR
      Do k = 1, size(U)
         R = R + U(k)**2
      End Do
      R = Sqrt(R)
    End Function norme2

  End Module mod_BFGS
  
