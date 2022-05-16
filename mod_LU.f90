Module mod_LU
   Use mod_variables

   Implicit None

Contains

   Subroutine LU(A, M, perm) !Effectue la décomposition LU de la matrice A dans la matrice M
      Real(PR), Dimension(:,:), Intent(In) :: A
      Real(PR), Dimension(Size(A,1),Size(A,1)), Intent(InOut) :: M
      Integer, Dimension(Size(A,1)), Intent(InOut) :: perm

      Real(PR), Dimension(Size(A,1)) :: ligne !Pour un éventuel échange de lignes
      Real(PR) :: min , x
      Integer :: i,j,k, n, i_pivot, p

      M = A
      n = Size(A,1)
      Do j=1, n-1
         !Trouver le pivot
         min = Abs( Abs(M(j,j)) - Abs(1./M(j,j)) )!Minimum au sens de la multiplication
         i_pivot = j
         Do i=j+1, n
            x = Abs( Abs(M(i,j)) - Abs(1./M(i,j)) )
            If (x < min) Then
               min = x
               i_pivot = i
            End If
         End Do
         !Echanger les lignes
         If (i_pivot /= j) Then
            ligne = M(i_pivot,:) ; M(i_pivot,:) = M(j,:) ; M(j,:) = ligne !Echange des lignes dans la matrice
            p = perm(i_pivot) ; perm(i_pivot) = perm(j) ; perm(j) = p !Echange des indices dans le tableau des permutations
         End If

         !Diviser la colonne par le pivot
         Do i=j+1, n
            M(i,j) = M(i,j)/M(j,j)
         End Do

         !Soustraire la sous-matrice
         Do i=j+1, n
            Do k=j+1, n
               M(i,k) = M(i,k) - M(i,j)*M(j,k)
            End Do
         End Do
      End Do

   End Subroutine LU

   Function reslu(A, B) Result(X)
      Real(PR), dimension(:,:), intent(in) :: A
      Real(PR), dimension(:), intent(in) :: B
      Real(PR), dimension(size(B)) :: X, Y, Z
      Integer :: i, k, n
      Real(PR) :: som
      Real(PR), dimension(Size(B),Size(B)) :: M
      Integer, Dimension(Size(B)) :: perm

      n = size(B)

      Do i=1, n
         perm(i) = i
      End Do

      Call LU(A, M, perm)

      Do i=1, n
         z(i) = b(perm(i))
      End Do

      ! Résolution de Ly=b
      y(1) = z(1) ! Cas i=1 traité à part
      Do i=2,n
         som = 0._PR
         Do k=1, i-1
            som = som + M(i,k)*y(k)
         End Do
         y(i) = z(i) - som
      End Do
      ! Résolution de Uz=y
      Do i=0,n-1
         som = 0._PR
         Do k=n, n-i+1, -1
            som = som + M(n-i,k)*x(k)
         End Do
         x(n-i) = ( y(n-i) - som )/M(n-i,n-i)
      End Do

   End Function reslu

End Module mod_LU
