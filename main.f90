Program main
  Use Newton
  Implicit None

  Character(20), Parameter :: fichier_sortie="out.dat"
  Real(PR), Parameter ::  Pi=4._PR*atan(1._PR), theta_max = 1._PR, d_theta=1.e-3
  
  Real(PR), Dimension(6) :: U ! U=(x,y,z,phi,psi,theta)
  
  U = (/ -8.83_PR, 50.02_PR, 10.83_PR, pi/2._PR, pi/2._PR, 0.96_PR /) !coordonnées de l'origine du repère "fémur" dans le repère "tibia"
  
  

  Open(111, File=fichier_sortie, Action="Write")
  Call Newtn(U)
  Write(111, *) U(1), U(2), U(3), U(6), U(4), U(5)! L'ordre x,y,z,theta,phi,psi est plus conventionnel
  Do While (U(6) < theta_max)
     U(6) = U(6) + d_theta
     Call Newtn(U)
     Write(111, *) U(1), U(2), U(3), U(6), U(4), U(5)! L'ordre x,y,z,theta,phi,psi est plus conventionnel
  End Do
  Close(111)
  
End Program main
