Module mod_LU

   Implicit None

Contains

   Subroutine decomp_lu(A, L, U)
      Real(PR), dimension(:,:), intent(in) :: A
      Real(PR), dimension(:,:), intent(out) :: L, U
      Real(PR), dimension(:,:), allocatable :: M
      Integer :: i, j, k, n

      n = size(A(1,:))
      Allocate(M(n,n))
      M = A

      Do k = 1, n-1
         M(k+1:n,k) = M(k+1:n,k) / M(k,k)
         Do j = k+1, n
            Do i = k+1, n
               M(i,j) = M(i,j) - M(i,k)*M(k,j)
            End Do
         End Do
      End Do

      Do k = 1, n
         L(k,1:k) = M(k,1:k)
         L(k,k:n) = 0
         L(k,k) = 1
         U(k,1:k) = 0
         U(k,k:n) = M(k,k:n)
      End Do
   End Subroutine decomp_lu

   Function solv_lu(A, B) Result(X)
      Real(PR), dimension(:,:), intent(in) :: A
      Real(PR), dimension(:), intent(in) :: B
      Real(PR), dimension(size(B)) :: X, Y
      Integer :: i, k, n
      Real(PR) :: som
      Real(PR), dimension(:,:), allocatable :: L, U

      n = size(B)
      Allocate(L(n,n), U(n,n))
      Call decomp_lu(A, L, U)

      Do i = 1, n
         som = 0._PR
         Do k = 1, i-1
            som = som + L(i,k)*Y(k)
         End Do
         Y(i) = (B(i)-som) / L(i,i)
      End Do

      Do i = n, 1, -1
         som = 0._PR
         Do k = i+1, n
            som = som + U(i,k)*X(k)
         End Do
         X(i) = (Y(i)-som) / U(i,i)
      End Do
   End Function solv_lu

End Module mod_LU
