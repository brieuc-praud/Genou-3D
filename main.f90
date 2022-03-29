Program main
  Use Newton
  Implicit None

  Real(PR), Parameter ::  Pi=4._8*atan(1._PR), theta_max = Pi/4._PR, d_theta=0.01
  Real(PR) :: x,y,z,phi,psi,theta
  

  x = -8.83_PR
  y = 50.02_PR
  z = 10.83_PR
  phi = 0._PR
  psi = 0._PR
  theta = pi/2._PR
  
  Call Newtn( theta, x, y, z, phi, psi )
  
  Do While (theta < theta_max)
     theta = theta + d_theta
     Call Newtn( theta, x, y, z, phi, psi )
  End Do
  
End Program main
