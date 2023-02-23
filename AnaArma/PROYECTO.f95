PROGRAM Programa_armaduras_TEAM

	!ELABOR� CAMPOS RAMON
	!FECHA DE LA �LTIMA MODIFICACI�N: 09 DE NOVIEMBRE DEL 2021.
	!OBJETIVO: C�LCULO DE DESPLAZAMIENTOS Y REACCIONES DE ELEMENTOS TIPO ARMADURA.
	!PRINCIPALES SECCIONES DEL PROGRAMA:
	!	1) Lectura del archivo de datos.
	!	2) Ensamble de la matriz de rigidez de la estructura.
	!	3) C�lculo de las submatrices de la matriz de rigidez.
	!	4) Ensamble del vector de Fuerzas Externas y el vector de desplazamientos prescritos.
	!	5) C�lculo de la inversa de Kuu.
	!	6) C�lculo del vector de desplazamientos.
	!	7) C�lculo del vector de reacciones.
	!UNIVERSIDAD AUT�NOMA DE YUCAT�N.
	!CAMPUS DE CIENCIAS EXACTAS E INGENIER�AS.
	!FACULTAD DE INGENIER�A.
	!MAESTR�A EN INGENIER�A.
	!OPCI�N ESTRUCTURAS.

IMPLICIT NONE

!DEFINIENDO DATOS DE ENTREDA AL PROGRAMA.
!	NGDLT: N�MERO DE GRADOS DE LIBERTAD TOTALES.
!	NGDLC: N�MERO DE GRADOS DE LIBERTAD CONOCIDOS.
!	NGDLD: N�MERO DE GRADOS DE LIBERTAD DESCONOCIDOS.
!	GDLCD: GRADOS DE LIBERTAD CONOCIDOS Y DESCONOCIDOS.
!	GDLC: GRADOS DE LIBERTAD CONOCIDOS.
!	GDLD: GRADOS DE LIBERTAD DESCONOCIDOS.
!	NPC: N�MERO DE POSICI�N CONOCIDO.
!	NPD: N�MERO DE POSICI�N DESCONOCIDO.

	CHARACTER(len=4):: NOMBREGRUPO
    CHARACTER(len=7):: tipo(50)
	INTEGER:: i, NNODO, NElEM, NMATE, NGEOM, NDIM, j, l

	INTEGER:: NODOi(50)
	REAL:: COORDXi(50), COORDYi(50)

    INTEGER:: ELEMi(50), MATi(50), GEOEi(50), NODOAi(50), NODOBi(50)
	CHARACTER(len=4):: TIPOEi(50)

	INTEGER:: MATEi(10)
	REAL:: Ei(10), Vi(10), reacs(50)

	INTEGER:: GEOi (10)
	REAL:: Ai (10)

	INTEGER:: NFUER, NFRON, NDESP
	INTEGER:: NODOFi(50)
	REAL:: FXi(50), FYi(50)

	INTEGER:: NODOFRi(50), FRONTERAXi(50), FRONTERAYi(50)

	INTEGER:: NODODESPi(50)
	REAL:: UXi(50), Vyi(50), asen(50)

	REAL:: KEST(50,50), LELEM(50), CONSE(50), KELEM(50,4,4), KUU(50,50), COSTETA(50), SENTETA(50)
    REAL::Fext(50), Fext2(50), KuuaAsen(50)
    REAL:: Kuua(50,50), Kuau(50,50), Kuaua(50,50), Ud(50),Ut(50),R(50), Fint(50,4), Faxial (50)     
    
	INTEGER:: NGDLT, NGDLC, NGDLD, GDLCD(50), GDLC(50), GDLD(50), NPC, NPD, posj,posi, pglobalx, pglobaly
    INTEGER:: CONTADOR, CONTADORASEN, x, y, contr



!LECTURA DE DATOS DE ARCHIVO "DATOS.TXT".

	OPEN(UNIT=11, FILE='DATOS.txt', STATUS='OLD', ACTION='READ')
	READ(11,*) NNODO, NELEM, NMATE, NGEOM, NDIM

	READ(11,*)
	READ(11,*) NOMBREGRUPO, NNODO
		DO i=1, NNODO
		READ(11,*) NODOi(i),COORDXi(i), COORDYi(i)
		END DO

	READ(11,*)
	READ(11,*) NOMBREGRUPO, NELEM
		DO i=1, NELEM
		READ(11,*) ELEMi(i), MATi(i), GEOEi(i), TIPOEi(i), NODOAi(i), NODOBi(i)
		END DO

	READ(11,*)
	READ(11,*) NOMBREGRUPO, NMATE
		DO i=1, NMATE
		READ(11,*) MATEi(i), Ei(i), Vi(i)
		END DO

	READ(11,*)
	READ(11,*) NOMBREGRUPO, NGEOM
		DO i=1, NGEOM
		READ(11,*) GEOi(i), Ai(i)
		END DO

	READ(11,*)
	READ(11,*) NOMBREGRUPO, NFUER
		DO i=1, NFUER
		READ(11,*) NODOFi(i), FXi(i), FYi(i)
		END DO

	READ(11,*)
	READ(11,*) NOMBREGRUPO, NFRON
		DO i=1, NFRON
		READ(11,*) NODOFRi(i), FRONTERAXi(i), FRONTERAYi(i)
		END DO

	READ(11,*)
	READ(11,*) NOMBREGRUPO, NDESP
		DO i=1, NDESP
		READ(11,*) NODODESPi(i), UXi(i), VYi(i)
		END DO
    

!INICIALIZANDO VALORES EN CERO PARA LAS POSICIONES DE LOS GRADOS DE LIBERTAD.

	NPC=0
	NPD=0
	NGDLC=0
	NGDLD=0

!OBTENIEDO GRADOS DE LIBERTAD TOTALES.

	NGDLT=NNODO*2

!HACIENDO CERO TODOS LOS GRADOS DE LIBERTAD.

	DO i=1, NGDLT
	GDLCD(i)=0
	END DO

!SE INTEGRA EL VECTOR CON UNOS Y CEROS QUE TIENE GRADOS DE LIBERTAD CONOCIDOS Y DESCONOCIDOS.

	DO i=1, NFRON
		IF (FRONTERAXi(i)==1) THEN
		GDLCD((nodoFri(i)*2)-1)=1
		END IF
	END DO

	DO i=1, NFRON
		IF (FRONTERAYi(i)==1) THEN
		GDLCD ((NODOFRi(i)*2))=1
		END IF
	END DO

!SE CONTABILIZA CUANTOS GRADOS DE LIBERTAD CONOCIDOS Y DESCONOCIDOS HAY.

	DO i=1, NGDLT
		IF (GDLCD(i)==1) THEN
		NGDLC=NGDLC+1
		END IF
	END DO
    
	NGDLD=NGDLT-NGDLC

!ENSAMBLANDO VECTORES CON GRADOS DE LIBERTAD CONOCIDOS Y VECTOR DE DESCONOCIDOS. SUS ELEMENTOS SON LAS POSICONES.

	NPC=0
	NPD=0

	DO i=1, NGDLT
		IF (GDLCD(i)==1) THEN
		NPC=NPC+1
		GDLC(NPC)=i
		ELSE
		NPD=NPD+1
		GDLD(NPD)=i
		END IF
	END DO

!SE HACEN CEROS LOS ELEMENTOS DE LA MATRIZ GLOBAL y las matrices de los elementos.

	DO i=1,50
		DO j=1,50
		KEST(i,j)=0
        
		END DO
	END DO

!SE OBTIENE LONGITUD DE CADA ELEMENTO PARA CADA MATRIZ DE RIGIDEZ LOCAL.

	DO i=1, NELEM
	LELEM(i)=SQRT(((COORDXi(NODOBi(i))-COORDXi(NODOAi(i)))**2)+((COORDYi(NODOBi(i))-COORDYi(NODOAi(i)))**2))
	END DO

!SE OBTIENEN VALORES DE COSENOS Y SENOS DEL �NGULO.

	DO i=1, NELEM
	COSTETA(i)=((COORDXi(NODOBi(i))-COORDXi(NODOAi(i)))/LELEM(i))
	END DO

	DO i=1, NELEM
	SENTETA(i)=((COORDYi(NODOBi(i))-COORDYi(NODOAi(i)))/LELEM(i))
	END DO  

!SE OBTIENEN CONSTANTES AE/L DE CADA ELEMENTO PARA CADA MATRIZ DE RIGIDEZ LOCAL.

	DO i=1, NELEM
	CONSE(i)= Ai(Geoei(i))*Ei(Mati(i))/LELEM(i)
	END DO     

!CALCULANDO VALORES DE MATRICES DE RIGIDEZ LOCALES.

	DO i=1, NELEM
	KELEM(i,1,1)=((COSTETA(i))**2)*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,2,1)=(COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,3,1)=(-(COSTETA(i))**2)*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,4,1)=(-COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,1,2)=(COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,2,2)=((SENTETA(i))**2)*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,3,2)=(-COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,4,2)=(-(SENTETA(i))**2)*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,1,3)=(-(COSTETA(i))**2)*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,2,3)=(-COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,3,3)=((COSTETA(i))**2)*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,4,3)=(COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,1,4)=(-COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,2,4)=(-(SENTETA(i))**2)*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,3,4)=(COSTETA(i))*(SENTETA(i))*(CONSE(i))
	END DO

	DO i=1, NELEM
	KELEM(i,4,4)=((SENTETA(i))**2)*(CONSE(i))
	END DO

!ENSAMBLE DE LA MATRIZ DE RIGIDEZ DE LA ESTRUCTURA.


do l=1 , nelem
  DO i=1,4
  	do j=1,4
	  if (i==1) THEN
	  posi = 2*NODOAi(l)-1
	  ELSE if (i==2) THEN
	  posi=2*NODOAi(l)
       ELSE if (i==3) THEN
	  posi=2*NODOBi(l)-1
      ELSE if (i==4) THEN
	  posi=2*NODOBi(l)
      end if
      
	If (j==1) THEN
      posj= 2*NODOAi(l)-1
	  ELSE if (j==2) THEN
	  posj=2*NODOAi(l)
      ELSE if (j==3) THEN
	  posj=2*NODObi(l)-1
      ELSE if (j==4) THEN
	  posj=2*NODObi(l)
    end if
	Kest(posi,posj)=kest(posi,posj)+kelem(l,i,j)
   end do
  end do
end do   


!SE HACEN CERO LAS MATRICES

         DO j=1,50 
           Kuu(i,j)   = 0.
           Kuua(i,j)  = 0.
           Kuau(i,j)  = 0. 
           Kuaua(i,j) = 0.
         END DO
    

!GRADOS DE LIBERTAD DESCONOCIDOS
!  C�lculo de la matriz Kuu
       DO i=1,ngdld
         DO j=1,ngdld
           Kuu(i,j) = Kest(gdld(i),gdld(j))
         END DO
       END DO
! calculo de la matriz kuu*
       DO i=1,ngdld
         DO j=1,ngdlc
           Kuua(i,j) = Kest(gdld(i),gdlc(j))
         END DO
       END DO
! calculo de la matriz ku*u
       DO i=1,ngdlc
         DO j=1,ngdld
           Kuau(i,j) = Kest(gdlc(i),gdld(j))
         END DO
       END DO
! calculo de la matriz ku*u*
       DO i=1,ngdlc
         DO j=1,ngdlc
           Kuaua(i,j) = Kest(gdlc(i),gdlc(j))
         END DO
       END DO

       
!! calculo de vector de fuerzas externas
do i=1,ngdld
Fext(i)=0
  end do
  
  do i= 1, NFUER
	pglobalx=2*(NODOFi(i))-1
	pglobaly=2*(NODOFi(i))
		do j=1, ngdld
			if (pglobalx== gdld(j)) THEN
			Fext(j)=Fxi(i)
			end if
			if (pglobaly== gdld(j)) THEN
			Fext(j)=Fyi(i)
			end if
		end do
end do

!! calculo del vector de asentamientos
do i=1,ngdlc
asen(i)=0
  end do
  
  do i= 1, NDESP
	pglobalx=2*(NODODESPi(i))-1
	pglobaly=2*(NODODESPi(i))
		do j=1, NGDLC
			if (pglobalx== gdlc(j)) THEN
			asen(j)=Uxi(i)
			end if
			if (pglobaly== GDLC(j)) THEN
			ASen(j)=Vyi(i)
			end if
		end do
end do


! ENSAMBLE DEL VECTOR DE FUERZAS EXTERNAS Y 
!           EL VETOR DE DESPLAZAMIENTOS PRESCRITOS

       !   Calcular el producto de [ Kuua ] { Uc }

 DO i=1,50
         KuuaAsen(i) = 0.
 END DO
       
       DO i=1,ngdld
         DO j=1,ngdlc
           KuuaAsen(i)=KuuaAsen(i)+(Kuua(i,j))*(asen(j))
         END DO
       END DO   

       !   Calcular el vector {Fext} - [ Kuua ] { Uc }
       DO i=1,ngdld
         Fext2(i) = Fext(i) - KuuaAsen(i)
       END DO   

  !  CÁLCULO DE LA MATRIZ INVERSA DE Kuu y determinación de los desplazamientos

   CALL Gaussj(Kuu, ngdld, 50, Fext2, 1, 1)


       !  CÁLCULO DEL VECTOR DE DESPLAZAMIENTOS
       
       !  Desplazamientos desconocidos
       DO i=1, ngdld
         Ud(i)=Fext2(i)
       END DO

       !  Desplazamientos totales


       
       CONTADOR = 0
       contadorasen=0
       DO i=1, ngdlt
        
         IF( gdlcd(i) == 0) THEN       ! GDLCD == 0 grado de libertad desconocido
           CONTADOR=CONTADOR+1
           Ut(i)=Ud(contador)
         ELSE 
           contadorasen=contadorasen+1                        ! Datogdl(i,2) == 1. grado de libertad conocido. 
           Ut(i)= Asen(contadorasen)         ! Se toma el dato de entrada
         END IF
       END DO


       !  CÁLCULO DEL VECTOR DE REACCIONES
DO i=1,ngdlc+1
  R(i)=0
  end do
      
       DO i=1,ngdlc
         DO j=1,ngdld        !   Calcular el producto de [ Kuau ] { Ud }
           R(i)=R(i)+(Kuau(i,j))*(Ud(j))
         END DO
         DO j=1,ngdlc
           R(i)=R(i)+(Kuaua(i,j))*(Asen(j))
         END DO
       END DO


! fuerzas internas
do i=1,nelem
	DO j=1,4
       Fint(i,j)=0
	end do
end do

do l=1 , nelem
  DO i=1,4
  	do j=1,4
	        
	If (j==1) THEN
	  posj= 2*NODOAi(l)-1
	  ELSE if (j==2) THEN
	  posj=2*NODOAi(l)
      ELSE if (j==3) THEN
	  posj=2*NODObi(l)-1
      ELSE if (j==4) THEN
	  posj=2*NODObi(l)
    end if
	Fint(l,i)=Fint(l,i)+ut(posj)*kelem(l,i,j)
   end do
  end do
end do   

!calculo de fuerza axial

do l=1, nelem
if (abs(costeta (l) )>0.1) then
  Faxial(l) = Fint(l,3)/costeta(l)
 else
   Faxial(l)= Fint(l,4)/senteta(l)
end if
end do


!ADECUACIÓN DE LA  variable R, PARA SU IMPRESIÓN
contr=0
do i=1, nfron
  if (fronteraxi(i)==1)then
    contr=contr+1
    reacs(i*2-1)=r(contr)
  end if
  if (fronteraxi(i)==0) then
      reacs(i*2-1)=0
  end if
  if (fronterayi(i)==1) then
    contr=contr+1
    reacs(i*2)=r(contr)
  end if
   if (fronterayi(i)==0) then
      reacs(i*2)=0
   end if
   
end do

!tipo de esfuerzo
	DO i=1, Nelem
         if (Faxial(i)>0) then
          tipo(i)='tension'
         end if
         if (Faxial(I)<0) then 
            tipo (i) = 'compres'         
         else if (Faxial(i)==0) then
          tipo(i)='s/esf'         
         end if  
    end do

!IMPRIMIENDO DATOS DE ENTRADA EN PANTALLA.

	PRINT*, "     "
	PRINT*, "     DATOS DE ENTRADA"

	PRINT*, "     "
	PRINT*, "     NODOS"
		DO i=1, NNODO
		PRINT*, NODOi(i), COORDXi(i), COORDYi(i)
		END DO

	PRINT*, "     "
	PRINT*, "     ELEMENTOS"
		do i=1, NELEM
		PRINT*, ELEMi(i), MATi(i), GEOEi(i), "     ", TIPOEi(i), "     ", NODOAi(i), NODOBi(i)
		END DO

	PRINT*, "     "
	PRINT*, "     MATERIALES"
		DO i=1, NMATE
		PRINT*, MATEi(i), Ei(i), Vi(i)
		END DO

	PRINT*, "     "
	PRINT*, "     GEOMETR�AS"
		DO i=1, NGEOM
		PRINT*, GEOi(i), Ai(i)
		END DO

	PRINT*, "     "
	PRINT*, "     FUERZAS"
		DO i=1, NFUER
		PRINT*, NODOFi(i), Fxi(i), Fyi(i)
		END DO

	PRINT*, "     "
	PRINT*, "     CONDICIONES DE FRONTERA"
		DO i=1, NFRON
		PRINT*, NODOFRi(i), FRONTERAXi(i), FRONTERAYi(i)
		END DO

	PRINT*, "     "
	PRINT*, "     DESPLAZAMIENTOS PREESCRITOS"
		DO i=1, NDESP
		PRINT*, NODODESPi(i), UXi(i), vyi(i)
		END DO

	PRINT*, "     "
   	PRINT*, "     GRADOS DE LIBERTAD CONOCIDOS Y DESCONOCIDOS"
		DO i=1, NGDLT
		PRINT*, GDLCD(i)
		END DO

	PRINT*, "     "
   	PRINT*, "     GRADOS DE LIBERTAD CONOCIDOS"
		DO i=1, NGDLC
		PRINT*, GDLC(i)
		END DO

	PRINT*, "     "
   	PRINT*, "     GRADOS DE LIBERTAD DESCONOCIDOS"
		DO i=1, NGDLD
		PRINT*, GDLD(i)
		END DO

!IMPRIMIENDO MATRICES DE RIGIDEZ LOCALES EN PANTALLA.

	PRINT*, "     "
	PRINT*, "     MATRICES DE RIGIDEZ LOCALES"
    DO i=1, NELEM
		PRINT*, KELEM(i,1,1), "   ", KELEM(i,1,2), "   ", KELEM(i,1,3), "   ", KELEM(i,1,4)
		PRINT*, KELEM(i,2,1), "   ", KELEM(i,2,2), "   ", KELEM(i,2,3), "   ", KELEM(i,2,4)
		PRINT*, KELEM(i,3,1), "   ", KELEM(i,3,2), "   ", KELEM(i,3,3), "   ", KELEM(i,3,4)
		PRINT*, KELEM(i,4,1), "   ", KELEM(i,4,2), "   ", KELEM(i,4,3), "   ", KELEM(i,4,4)
		PRINT*, "     "
    END DO

do i=1, nnodo*2
  do j=1, nnodo*2
	PRINT*, KEST(I,J), "    "
  end do 
  print*, "   "
end do 
 print*, "   "
    print*, " FIN DE LA MATRIZ GLOBAL  "
     print*, "   "
      print*, "   "

	pRINT*,"  "
   DO i=1, NGDLD
     DO j=1, NGDLD
		PRINT*, KUU(i,j)
    END DO
pRINT*, "         "
END DO
print*, "   "
print*, "  fin de Kuu"
print*, "   "



do j=1, ngdld
   print*, Fext(j)
  print*, "   "
end do 
print*, "   "
print*, "  fin "
print*, "   "
 do j=1, ngdlc
   print*, asen(j)
  print*, "   "
end do   
 print*, "   "
print*, " fiN ASEN  "
print*, "   "
print*, "  INICIO UD "

do j=1, ngdld
   print*, Ud(j)
  print*, "   "
end do    

print*, "  FIN UD  "
print*, "   "
print*, "  INICIO UT "
do j=1, ngdlT
   print*, UT(j)
  print*, "   "
end do  

print*, "  FIN UT  "
print*, "   "
print*, "  INICIO R "
do j=1, ngdlC
   print*, R(j)
  print*, "   "
end do  
print*, "   "
print*, "   "
do i=1, nelem
print*, tipo(i)
end do
!IMPRIMIENDO RESULTADOS EN UN ARCHIVO "RESULTADOS.TXT".

	OPEN(UNIT=11, FILE="RESULTADOS.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '     '
	WRITE(11,*) '                 RESULTADOS'
	WRITE(11,*) "    "

	
WRITE(11,*) "     "
	WRITE(11,*) "              DESPLAZAMIENTOS"
    WRITE(11,*) "          NODO           UX               UY"
		DO i=1, NNODO  
        x=i*2-1
        y=i*2
		WRITE(11,*)I, "   " ,UT(x), "  ", UT(y)
		END DO
WRITE(11,*) "     "
	WRITE(11,*) "             REACCIONES"
    WRITE(11,*) "         NODO          RX                RY"
		
         do i  = 1, nfron
            x=i*2-1
        y=i*2
		WRITE(11,*)NODOFRI(I), "   " ,Reacs(x), "  ", reacs(y)
		END DO	

WRITE(11,*) "     "
	WRITE(11,*) "             FUERZAS INTERNAS"
    WRITE(11,*) "        ELEMENTO     Fxa             Fya             Fxb             Fyb            Faxial         tipo"
		DO i=1, Nelem
        
		WRITE(11,*)Elemi(I), " " ,Fint(i,1), " ", Fint(i,2), " ", Fint(i,3), " ", Fint(i,4), " " , Faxial(i) , " ", tipo(i), "   ok"
		END DO	



CLOSE(11)

END PROGRAM Programa_armaduras_TEAM



   !  Linear equation solution by Gauss-Jordan elimination... 
   !  a(1:n,1:n) is an input matrix stored in an array of physical dimensions np by np. 
   !  b(1:n,1:m) is an input matrix containing the m right-hand side vectors, 
   !  stored in an array of physical dimensions np by mp. 
   !
   !  On output, a(1:n,1:n) is replaced by its matrix inverse, 
   !  and b(1:n,1:m) is replaced by the corresponding set of solution vectors. 
   !
   !  Can handle up to dimension=50. NR bug: mp is never used; thus, it can be anything -- simply set "mp=1".)


SUBROUTINE Gaussj (a,n,np,b,m,mp)
IMPLICIT NONE
INTEGER m, mp, n, np, NMAX
REAL a(np,np), b(np,mp)
PARAMETER (NMAX=50)
INTEGER i, icol, irow, j, k, l, ll, indxc(NMAX), indxr(NMAX), ipiv(NMAX)
REAL big, dum, pivinv

ipiv = 0

DO i=1,n 
	big = 0.0
	DO j = 1, n
    	IF (ipiv(j)/=1) then 
        	DO k = 1,n
            	IF (ipiv(k) == 0) THEN 
                	IF (abs (a(j,k))> big) THEN 
                    big = abs (a(j,k))
                    irow = j
                    icol = k
                    END IF 
                END IF 
            END DO 
        END IF	
	END DO
	ipiv(icol) = ipiv(icol) + 1
	
	IF (irow/=icol) THEN 
    	DO l=1,n
        	dum = a(irow,l)
	        a(irow,l)=a(icol,l)
    	    a(icol,l) = dum 
        END DO
        DO l=1,m
        	dum = b(irow,l)
            b(irow,l) = b(icol,l)
            b(icol,l) = dum   
        END DO  
    END IF
	
	indxr(i) = irow
    indxc(i) = icol 

    IF (a(icol,icol) == 0) THEN
	PRINT *, 'MATRIZ SINGULAR' 
	END IF
      
   	pivinv = 1.0 / a(icol,icol)
    a(icol,icol) = 1
    
    DO l =1,n
	a(icol,l) = a(icol,l)*pivinv
    END DO 
    
    DO l =1,m
	b(icol,l) = b(icol,l)*pivinv
    END DO

    DO ll=1,n
    	IF (ll /= icol)THEN
        	dum = a(ll,icol)
            a(ll,icol) = 0
            	DO l = 1,n
                	a(ll,l) = a(ll,l) - a(icol,l)*dum
                END DO  
                
                DO l=1,m
                	b(ll,l) = b(ll,l) - b(icol,l)*dum  
                END DO   
        END IF
    END DO
END DO 

DO l = n,1, -1
	IF (indxr(l) /= indxc(l)) THEN
    	DO k = 1, n
        dum = a(k,indxr(l))
        a(k,indxr(l))= a(k,indxc(l))
        a(k,indxc(l))= dum
        END DO 
    END IF                 
END DO

END SUBROUTINE Gaussj



