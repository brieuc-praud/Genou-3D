Module mod_recuit
   Use mod_variables
   Use mod_erreur
   Implicit None

Contains

   Subroutine RecSim(U, theta)
      Real(PR), Parameter :: emin=1_PR, n=1e3_PR

      Real(PR), Dimension(5), Intent(InOut) :: U !U = (x,y,z,phi,psi)
      Real(PR), Intent(In) :: theta

      Real(PR) :: eu, ev ! énergies
      Real(PR) :: T ! température
      Real(PR), Dimension(5) :: V ! V voisin
      Integer :: k ! incrément de boucle

      eu = erreur(U, theta)
      k = 2
      Do While (k < n .AND. eu > emin)
         T = 10._PR / log( Real(k,PR) ) ! Loi de décroissance de T

         V = voisin(U)
         ev = erreur(V, theta)
         If ( ev < eu .OR. proba(ev-eu,T) ) Then
            U = V
            eu = ev
         End If
         k = k+1
      End Do
   End Subroutine RecSim

   Function voisin(U) Result(V)
      ! Renvoie un voisin aléatoire de l'état U
      Real(PR), Dimension(5), Intent(In) :: U ! U = (x,y,z,phi,psi)

      Real(PR), Dimension(5) :: V ! V = (x,y,z,phi,psi)

      Integer :: i
      Real(PR) :: rand

      Do i=1, 5
         Call random_number(rand) ! aléatoire uniforme entre 0 et 1
         rand = 2._PR * (rand - 0.5_PR) ! aléatoire uniforme entre -x et x
         V(i) = U(i) + rand
      End Do
   End Function voisin

   Function proba(deltaE, temperature)
      ! Probabilité de garder le nouvel état dans le recuit
      Real(PR), Intent(In) :: deltaE, temperature
      Logical :: proba

      Real(PR) :: rand

      Call random_number(rand) ! aléatoire uniforme entre 0 et 1

      proba = rand < exp(-deltaE/temperature)

   End Function proba

End Module mod_recuit
