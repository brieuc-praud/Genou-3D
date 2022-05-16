Program main
   Use mod_BFGS
   Use mod_recuit
   Implicit None

   Character(20), Parameter :: fichier_u="u.dat", fichier_quat="quat.dat"
   Real(PR), Parameter :: theta_max = 1.5_PR, d_theta=5.e-3


   Real(PR), Dimension(5) :: U ! U=(x,y,z,phi,psi)
   Real(PR) :: theta
   Integer :: methode ! Numéro de la méthode à utiliser (BFGS ou Recuit simulé)

   ! Pour les données du fichier "quat.dat"
   Type(quaternion), Dimension(10) :: quat_list
   Character(1000) :: str
   Integer :: j

   Write(*, *) "Méthode à utiliser :"
   Write(*, *) "0 : BFGS (ne converge pas)"
   Write(*, *) "1 : Recuit simulé"
   Read(*, *) methode

   U = (/ -8.83_PR, 50.02_PR, 10.83_PR, -0.24_PR, 0.14_PR /) !coordonnées de l'origine du repère "fémur" dans le repère "tibia"
   theta = 0.96_PR

   Open(111, File=fichier_u, Action="Write") ! Pour l'animation Blender
   Open(112, File=fichier_quat, Action="Write") ! Pour l'animation Python

   Select Case (methode)
    Case (1)
      Call RecSim(U, theta)
    Case DEFAULT
      Call BFGS(U, theta)
   End Select

   ! Pour le fichier "u.dat"
   Write(111, "(F17.10,F17.10,F17.10,F17.10,F17.10,F17.10)" &
   & ) U(1), U(2), U(3), theta, U(4), U(5) ! L'ordre x,y,z,theta,phi,psi est plus conventionnel

   ! Pour le fichier "quat.dat"
   quat_list = calculer_positions(U, theta)
   str = ""
   Do j=1, 10
      str = trim(str)//trim( print_quat(quat_list(j)) )
   End Do
   Write(112, *) str

   Do While (theta < theta_max)
      theta = theta + d_theta
      Select Case (methode)
       Case (1)
         Call RecSim(U, theta)
       Case DEFAULT
         Call BFGS(U, theta)
      End Select
      ! Pour le fichier "u.dat"
      Write(111, "(F17.10,F17.10,F17.10,F17.10,F17.10,F17.10)" &
      & ) U(1), U(2), U(3), theta, U(4), U(5) ! L'ordre x,y,z,theta,phi,psi est plus conventionnel
      ! Pour le fichier "quat.dat"
      quat_list = calculer_positions(U, theta)
      str = ""
      Do j=1, 10
         str = trim(str)//trim( print_quat(quat_list(j)) )
      End Do
      Write(112, *) str
   End Do

   Close(111)
   Close(112)

End Program main
