PROGRAM TAREA_4_DEA
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

    INTEGER:: I
	REAL:: T, BETA, GAMA, M,K, W,DT, C,XI,A(20000),KD

! COEFICIENTES
    REAL::A1,A2,A3
 
!!!!!VALORES DE SALIDA   
!F:FUERZA 
!FD:FUERZA DIN�MICA
!XR: DESPLAZAMIENTO RELATIVO
!VR: VELOCIDAD RELATIVA
!AR: ACELERACI�N RELATIVA
!AT: ACELERACI�N TOTAL

    REAL::F(20000),FD(100000), XR(100000),VR(100000),AR(100000), AT(100000)
    
!!!!!VARIABLES AUXILIARES
    INTEGER:: J, N, h 
    REAL::  x , y,XRabs(100000),VRabs(100000),ARabs(100000),ATabs(100000), MaxXR(100000), D, TM 
    REAL::MaxVR(100000) ,MaxAR(100000), MaxAT(100000)

!!!!!!DEFINICI�N DE CONSTANTES
    
GAMA=0.5   !NO CAMBIA  

!LECTURA DEL ARCHIVO DE ENTRADA
	OPEN(UNIT=11, FILE='SCT.txt', STATUS='OLD', ACTION='READ')
	READ(11,*) 
	READ(11,*)XI
    READ(11,*)
    READ(11,*)BETA, M, I, DT 
    READ(11,*)
    READ(11,*)
    READ(11,*)
		DO j=1, I
		READ(11,*) x, A(J), y
		END DO
      
CLOSE(11)

D=120  !! n�mero de datos 
TM=6   !! periodo m�ximo a obtener

!! calculos de valores
do h=1, D
  T=(TM/D)*h  !! ir� de 0.25 en 0.25 hasta 6s

  !C�LCULO DE K
  W=(2*3.1416)/T
  K=(W**2)*M

  !C�LCULO DE COEFICIENTE DE AMORTIGUAMIENTO
  C=2*M*W*XI

  !C�LCULO DE LOS COEFICIENTES
  A1=((M/(BETA*(DT**2)))+ (GAMA*C)/(BETA*DT))
  A2=(M/(BETA*DT))+((GAMA/BETA)-1)*C
  A3=(((1/(2*BETA))-1)*M)+(C*DT*((GAMA/(2*BETA))-1))

  KD=K+A1
  N=i
 
  !C�LCULO DEL VECTOR DE FUERZAS 
  DO j=1, N
    F(J)=A(J)*(-1)
  END DO

  !C�LCULO DE VALORES INICIALES
  FD(1)=F(1)
  XR(1)=0
  VR(1)=0
  AR(1)=F(1)



  !C�LCULO DEL VECTOR DE FUERZA DIN�MICA, DE DESPLAZAMIENTOS RELATIVOS, DE VELOCIDAD RELATIVA Y DE ACELERACI�N RELATIVA
  DO J=1, N
    IF (J>1) THEN 
    FD(J)=F(J)+(A1*XR(J-1))+(A2*VR(J-1))+(A3*AR(J-1))
    XR(J)=FD(J)/KD
    VR(J)=(GAMA/(BETA*DT))*(XR(J)-XR(J-1))+(1-(GAMA/BETA))*VR(J-1)+(1-(GAMA/(2*BETA)))*DT*AR(J-1)
    AR(J)=(1/(BETA*DT**2))*(XR(J)-XR(J-1))-((1/(BETA*DT))*VR(J-1))-(((1/(2*BETA))-1)*AR(J-1))
    END IF
  END DO


  !C�LCULO DEL VECTOR DE ACELERACIONES TOTALES
  DO j=1, N
    AT(J)=AR(J)+A(J)
  END DO
  !!Conversi�n de los negativos a positivos
  DO j=1, N
    XRabs(J)=abs(XR(J)) 
    VRabs(J)=abs(VR(J))
    ARabs(J)=abs(AR(J))
    ATabs(J)=abs(AT(J))
  END DO

 !! obtenci�n de vectores con los valores m�ximos  
  MaxXR(h)=maxval(XRabs) !!dezplazamientos
  MaxVR(h)=maxval(VRabs) !!velocidades
  MaxAR(h)=maxval(ARabs) !!aceleraciones relativas
  MaxAT(h)=maxval(ATabs) !!aceleraciones m�ximas

end do !!fin del do con h

PRINT*, MaxXR(20), FD(2), XR(2),K, KD ,A1, A2, A3, C, M, DT, AT(3), (C)

  PRINT*, '   RESULTADOS ' 
  PRINT*, '         I          F            FD             XR            VR            AR             AT'
DO J=1, 20
  PRINT*, J*DT, F(J), FD(J), XR(J), VR(J), AR(J),AT(J)
END DO 


!IMPRIMIENDO RESULTADOS EN UN ARCHIVO "RESULTADOS.TXT".

	OPEN(UNIT=11, FILE="RESULTADOS.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '     '
	WRITE(11,*) '                                                    RESULTADOS'
	WRITE(11,*) "    "

	
WRITE(11,*) "     "
	WRITE(11,*) "    " 
    WRITE(11,*)'         I             XR max         VR max         AR max          AT max'
		DO J=1, D
		WRITE(11,*) (J*(TM/D)) , MaxXR(J), MaxVR(J), MaxAR(J), MaxAT(J)
		END DO
CLOSE(11)
        
END PROGRAM TAREA_4_DEA