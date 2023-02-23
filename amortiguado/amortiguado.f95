PROGRAM TAREA_5_DEA
IMPLICIT NONE
!DEFINICIÓN DE LAS VARIABLES
!T: PERIODO 
!I: NÚMERO DE DATOS
!A: ACELERACIÓN LEIDA 


    INTEGER:: I
	REAL:: T, S
    real :: A(100000)
    complex:: Acomp(100000)
 
!!!!!VALORES DE SALIDA   
 

    Complex::Aconj(100000), amp(100000)
    
!!!!!VARIABLES AUXILIARES
    INTEGER:: J, N, h 
    REAL::  x , y

!LECTURA DEL ARCHIVO DE ENTRADA
	OPEN(UNIT=11, FILE='SCT.txt', STATUS='OLD', ACTION='READ')
	READ(11,*) 	
    READ(11,*)I, N
    READ(11,*)
    READ(11,*)
    READ(11,*)
		DO j=1, I
		READ(11,*) x, A(J), y
		END DO
      
CLOSE(11)


!!AGREGA CEROS A LOS VALORES DE LA ACELARACIÓN 
 DO j=I, N
   A(J)=0
 END DO
 
!! se convierte el vector en numeros complejos
do j=1, N
AComp(j)=CMPLX(A(J))
end do
S=-1.0  !! para transformada
  T=2

!EMPLEO DE LA SUBRUTINA

   CALL FORK (N,S,Acomp)
   do j=1, N
   Aconj(j)=CONJG(Acomp(j))
   end do
  
    do j=1,N
      amp(j)=SQRT(Acomp(j)*Aconj(j)) 
    END DO 
      do j=1,20 
        PRINT*, A(j), Acomp(j), amp(j)
      end do


      
!IMPRIMIENDO RESULTADOS EN UN ARCHIVO "RESULTADOS.TXT".

	OPEN(UNIT=11, FILE="SCT FRECUENCIAS.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '     '
	WRITE(11,*) '                                               sct  RESULTADOS '
	WRITE(11,*) "    "

	
WRITE(11,*) "     "
	WRITE(11,*) "    " 
    WRITE(11,*)'       ax         HERTZ          AMP    '
		DO J=1, N
		WRITE(11,*)  A(j), Acomp(j), amp(j)
		END DO
CLOSE(11)
        
END PROGRAM TAREA_5_DEA

 SUBROUTINE FORK (LX,SIGNI,CX)

      integer lx 
     integer name
      real signi 
      COMPLEX CX(LX)
      complex CARG,CTEMP,CW

     common /nmalloc/name(2048)

      J=1
      SC=SQRT(1.0/LX)
      DO 30 I=1,LX
      IF (I .GT. J) GOTO 10   !! GREATHER THAN
      CTEMP=CX(J)*SC
      CX(J)=CX(I)*SC
      CX(I)=CTEMP
   10 M=LX/2
   20 IF (J .LE. M) GOTO 30  !!LESS OR EQUAL
      J=J-M
      M=M/2
      IF (M .GE. 1) GOTO 20 !! GREATER OR EQUAL
   30 J=J+M
      L=1
   40 ISTEP=2*L
      DO 50 M=1,L
      CARG=(0.0,1.0)*(3.1415926536*SIGNI*(M-1))/L
      CW=CEXP(CARG)
      DO 50 I=M,LX,ISTEP
      CTEMP=CW*CX(I+L)
      CX(I+L)=CX(I)-CTEMP
   50 CX(I)=CX(I)+CTEMP
      L=ISTEP
      IF (L .LT. LX) GOTO 40  !!LESS THAN
      RETURN
      END
