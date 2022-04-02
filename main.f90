Program main
  Use mod_BFGS
  Implicit None

  Character(20), Parameter :: fichier_sortie="out.dat"
  Real(PR), Parameter ::  Pi=4._PR*atan(1._PR), theta_max = 0.97_PR, d_theta=1.e-3
  
  Real(PR), Dimension(5) :: U ! U=(x,y,z,phi,psi)
  Real(PR) :: theta
  
  U = (/ -8.83_PR, 50.02_PR, 10.83_PR, pi, pi /) !coordonnées de l'origine du repère "fémur" dans le repère "tibia"
  theta = 0.96_PR
  
  
  Open(111, File=fichier_sortie, Action="Write")
  !Call Newtn(U)
  Print *, "theta=", theta
  Call BFGS(U, theta)
  Write(111, *) U(1), U(2), U(3), theta, U(4), U(5)! L'ordre x,y,z,theta,phi,psi est plus conventionnel
  Do While (theta < theta_max)
     Print *, "theta=", theta
     theta = theta + d_theta
     !Call Newtn(U)
     Call BFGS(U, theta)
     Write(111, *) U(1), U(2), U(3), theta, U(4), U(5)! L'ordre x,y,z,theta,phi,psi est plus conventionnel
  End Do
  Close(111)
  
End Program main
