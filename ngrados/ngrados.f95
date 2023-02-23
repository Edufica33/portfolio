PROGRAM TAREA_7_DEA
IMPLICIT NONE
!DEFINICI�N DE LAS VARIABLES
!T: PERIODO 
!I: N�MERO DE DATOS
!M:MASA
!K:RIGIDEZ
!C: COEFICIENTE DE AMORTIGUAMIENTO
!BETA:COEFICIENTE BETA
!GAMA:COEFICIENTE GAMA  
!W: FRECUENCIA NATURAL
!DT: DIFERENCIAL DE TIEMPO
!X0: POSICION INICIAL
!V0: VELOCIDAD INICIAL
!XI:PORCENTAJE DE AMORTIGUAMIENTO
!A: ACELERACI�N LEIDA 
!KD: RIGIDEZ DIN�MICA
!NG: NUMEROS DE GRADOS DE LIBERTAD

    INTEGER:: I, NG
	REAL:: T(100), BETA, GAMA, M (100,100),K(100,100), W(100,100),DT, C(100,100),XI,A(20000),KD(100,100)

! COEFICIENTES
    REAL::A1(100,100),A2(100,100),A3(100,100)
 
!!!!!VALORES DE SALIDA   
!F:FUERZA 
!FD:FUERZA DIN�MICA
!XR: DESPLAZAMIENTO RELATIVO
!VR: VELOCIDAD RELATIVA
!AR: ACELERACI�N RELATIVA
!AT: ACELERACI�N TOTAL

    REAL::F(20000),FD(100,20000), XR(100,20000),VR(100,20000),AR(100,20000), AT(100,20000)
    
!!!!!VARIABLES AUXILIARES
!AK: almacena los valores de rigidez
!AM: almacena los valores de masas
!AW: almacena los valores de las frecuencias
    INTEGER:: J, N,O
    REAL:: MR(20000,20000), x , y, AK(100), AM(100), AW(100)

!!!!!!DEFINICI�N DE CONSTANTES
    
GAMA=0.5   !NO CAMBIA PARA CONSTANTE

!LECTURA DEL ARCHIVO DE ENTRADA
	OPEN(UNIT=11, FILE='SCT.txt', STATUS='OLD', ACTION='READ')
	READ(11,*) 
	READ(11,*)XI
	READ(11,*) 
    READ(11,*)NG
    READ(11,*)
    DO J=1,NG
      READ(11,*)AK(J)
    END DO  
    READ(11,*)
    DO J=1,NG
      READ(11,*)AM(J)
    END DO  
    READ(11,*)
    READ(11,*)BETA, I, DT
    READ(11,*)
    DO J=1,NG
      READ(11,*)T(J)
    END DO 
    READ(11,*) 
    DO J=1,NG
      READ(11,*)AW(J)
    END DO  
    READ(11,*)
    READ(11,*)
    READ(11,*)
		DO J=1, I
		READ(11,*) x, A(J),  y
		END DO
      
CLOSE(11)

!CREACI�N DE LAS MATRICES DIAGONALES Y HACIENDO CEROS LAS QUE NO NECESARIAMENTE TENDR�N TODOS SUS VALORES
DO O=1,NG  !HACE CERO TODO
  DO J=1,NG
    M(O,J)=0
    K(O,J)=0
    W(O,J)=0
    A1(O,J)=0
    A2(O,J)=0
    A3(O,J)=0
    KD(O,J)=0
    
  END DO
END DO

DO O=1,20  !HACE CERO TODO (M�XIMO 20 ELEMENTOS SE IMPRIMEN
  DO J=1,I
    XR(O,J)=0
    VR(O,J)=0
    AR(O,J)=0
    AT(O,J)=0
END DO
END DO


DO J=1,NG           !ASIGNA LOS VALORES A LAS DIAGONALES
  M(J,J)=AM(J)
  K(J,J)=AK(J)
  W(J,J)=AW(J)
END DO

!C�LCULO DEL VECTOR DE COEFICIENTES DE AMORTIGUAMIENTO(DIAGONALIZADO)

DO J=1,NG
  C(J,J)=2*M(J,J)*W(J,J)*XI
END DO

!C�LCULO DE LOS COEFICIENTES (TODAS SON MATRICES DIAGONALIZADAS)
DO J=1,NG
  A1(J,J)=((M(J,J)/(BETA*DT**2))+ GAMA*C(J,J)/(BETA*DT))
  A2(J,J)=((M(J,J)/(BETA*DT))+((GAMA/BETA)-1)*C(J,J))
  A3(J,J)=((((1/(2*BETA))-1)*M(J,J))+(C(J,J)*DT)*((GAMA/(2*BETA))-1))

  KD(J,J)=K(J,J)+A1(J,J)
END DO
N=i
 
!C�LCULO DEL VECTOR DE FUERZAS 
DO j=1, N
F(J)=A(J)*(-1)
END DO

!C�LCULO DE VALORES INICIALES
DO O=1,NG
  FD(O,1)=F(1)
  XR(O,1)=0
  VR(O,1)=0
  AR(O,1)=F(1)
END DO

!C�LCULO DEL VECTOR DE FUERZA DIN�MICA, DE DESPLAZAMIENTOS RELATIVOS, DE VELOCIDAD RELATIVA Y DE ACELERACI�N RELATIVA
DO O=1, NG
  DO J=1,N
    IF (J>1) THEN 
    FD(O,J)=F(J)+(A1(O,O)*XR(O,J-1))+(A2(O,O)*VR(O,J-1))+(A3(O,O)*AR(O,J-1))
    XR(O,J)=FD(O,J)/KD(O,O)
    VR(O,J)=(GAMA/(BETA*DT))*(XR(O,J)-XR(O,J-1))+(1-(GAMA/BETA))*VR(O,J-1)+(1-(GAMA/(2*BETA)))*DT*AR(O,J-1)
    AR(O,J)=(1/(BETA*DT**2))*(XR(O,J)-XR(O,J-1))-((1/(BETA*DT))*VR(O,J-1))-(((1/(2*BETA))-1)*AR(O,J-1))
    END IF
  END DO
END DO


!C�LCULO DEL VECTOR DE ACELERACIONES TOTALES
DO O=1,NG
  DO J=1, N
    AT(O,J)=AR(O,J)+A(J)
  END DO
END DO


DO J=1,NG
PRINT*,  A1(J,J), A2(J,J), C(J,J), M(J,J), KD(J,J)
END DO
PRINT*, KD(1,2)
  PRINT*, '   RESULTADOS ' 
  PRINT*, '         I          F            FD             XR            VR            AR             AT'

  PRINT*, J*DT, F(10), FD(2,10), XR(2,4), VR(1,1000), AR(1,500),AT(1,2000) 


OPEN(UNIT=11, FILE="RESULTADOS 1-5.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '     '
	WRITE(11,*) '                                                    RESULTADOS'
	WRITE(11,*) "    "

	
WRITE(11,*) "     "
WRITE(11,*) "    " 
WRITE(11,*)'            I               1              2              3              4               5    '
	  DO J=1, N  
        WRITE(11,*) J*DT,VR(1,J),VR(2,J),VR(3,J),VR(4,J),VR(5,J)
	 END DO
CLOSE(11)

IF (NG > 5) THEN
OPEN(UNIT=11, FILE="RESULTADOS 6-10.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '     '
	WRITE(11,*) '                                                    RESULTADOS'
	WRITE(11,*) "    "

	
WRITE(11,*) "     "
WRITE(11,*) "    " 
WRITE(11,*)'            I               6              7              8              9                10   '
	  DO J=1, N  
        WRITE(11,*) J*DT, VR(6,J),VR(7,J),VR(8,J),VR(9,J),VR(10,J)
	 END DO
CLOSE(11)
END IF

IF (NG > 10) THEN
OPEN(UNIT=11, FILE="RESULTADOS 11-15.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '     '
	WRITE(11,*) '                                                    RESULTADOS'
	WRITE(11,*) "    "

	
WRITE(11,*) "     "
WRITE(11,*) "    " 
WRITE(11,*)'            I               11             12              13              14                15   '
	  DO J=1, N  
        WRITE(11,*) J*DT, VR(11,J),VR(12,J),VR(13,J),VR(14,J),VR(15,J)
	 END DO
CLOSE(11)
END IF

        
END PROGRAM TAREA_7_DEA