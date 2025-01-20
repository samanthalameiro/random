PROGRAM monte_gato

   IMPLICIT NONE
   
   INTEGER :: i, sul_count, total_simulacoes
   REAL :: p
   CHARACTER :: direcao
   
   total_simulacoes = 100 
   sul_count = 0
   
      DO i = 1, total_simulacoes
         
         CALL andar_gato(direcao)
         
            IF (direcao == 'S') THEN 
    
                sul_count = sul_count + 1 !Número de vezes que o Berga foi pro sul
                
            ENDIF
            
      ENDDO
      
      p = real(sul_count) / real(total_simulacoes)
      
      PRINT*, "Probabilidade do Bergamota ir para o sul:", p

END PROGRAM monte_gato

SUBROUTINE andar_gato(direcao)
   
   IMPLICIT NONE
   
   REAL :: escolha
   CHARACTER :: direcao 
   
   CALL random_seed()
   
   CALL random_number(escolha)
   
   SELECT CASE (INT(4 * escolha) + 1) 
      
      CASE (1) 
         direcao = 'N' 
      
      CASE (2)
         direcao = 'S'
      
      CASE (3)
         direcao = 'L'
      
      CASE (4)
         direcao = 'O'
         
   ENDSELECT

END SUBROUTINE andar_gato  
   
   
   
