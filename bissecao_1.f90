MODULE pass

   IMPLICIT NONE
   
   REAL :: a, b

CONTAINS

REAL FUNCTION f(x)
   
   REAL, INTENT(IN) :: x
   
   f = x**3 - 6*x**2 - x + 30 

END FUNCTION f
   
END MODULE pass

PROGRAM Definir

   USE pass
   
   REAL :: c, erro
   INTEGER :: k
   
   CALL Local()
   
   CALL Bissecao()
   
   
END PROGRAM Definir

SUBROUTINE Local()

   USE pass
  
   REAL :: x, passo
   INTEGER :: i
   
   x = -100.
   
   passo = 1e-4
   
   i = 0
   
   DO WHILE (x <= 5)
   
      a = x
      
      x = x + passo
      
      b = x
      
      i = i + 1   
      
      IF (f(a)*f(b) < 0) THEN
      
         PRINT*, "Há raízes no intervalo:", a, b
      
         EXIT
      
      ENDIF
     
   ENDDO
   
   PRINT*, "Número de iterações:", i

END SUBROUTINE Local


SUBROUTINE Bissecao()

   USE pass

   REAL :: tol
   
   tol = 1e-3
   
   k = 0
   
   kmax = 20
   
   erro = 1e-3
   
   IF (f(a)*f(b) < 0) THEN
   
      DO
      
         c = (a + b)/2
      
         IF (f(a)*f(c) < 0) THEN
         
            b = c
         
         ELSE
         
            a = c
         
         END IF
            
         erro = abs(f(c))
   
         k = k + 1
         
         PRINT*, "(c):     Erro:    (k):"
   
         PRINT 12, c, erro, k
        
         12 FORMAT(F9.6, F9.6, I3)
            
         IF ((erro <= tol) .or. (k >= kmax)) EXIT
           
      ENDDO
         
   ELSE
      
   PRINT*, "Não foram encontradas raízes"
      
   END IF

END SUBROUTINE Bissecao




