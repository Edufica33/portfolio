!!Programa para aplciar el método de Pretley
!los datos de entrada son los puntos de la envolvente (desplazamiento y carga)hasta la carga máxima, Numero de datos experimentales, el intervalo de discretización
! los datos se introducen en un archivo llamado datos.txt y el resultado se entrega en un documento llamado resultado.txt
!SI SE USARÁN DATOS NEGATIVOS, SE TIENEN QUE USAR SUS VALORES ABSOLUTAS EN EL DOCUMENTO DE DATOS.TXT
PROGRAM PRIESTLEY
IMPLICIT NONE
    INTEGER:: i,j, cont,conti, SUMI, NDExp,NDDis                !auxiliar, auxiliar, auxiliar, NUMERO DE PUNTOS EXPERIMENTALES, NUM DE DATOS DISCRETIZADOS,
	  REAL:: Envolvente(1000,1000), Intervalo                         !DATOS DE LA ENVOLVENTE (DESPLAZAMIENTO, CARGA),  INTERVALO DE DISCRETIZACION
    REAL:: EnvolventeDis(13000,13000),Recta1 (13000,13000), M1,M2 !DATOS DE LA ENVOLVENTE DISCRETIZADOS (DESPLAZAMIENTO, CARGA), PUNTOS DE LA RECTA QUE PASA POR EL 0.75FY, PENDIENTE DE RECTA1, PEND. RECTA 2
    REAL:: Recta2 (13000,13000),DeltaD (13000),SFY(1,2),FY(1,2)   !PUNTOS DE LA RECTA QUE PASA POR LA CARGA MÁXIMA, DIFERENCIA DE DESPLAZAMIENTO, PUNTO DEL 0.75FY, PUNTO DEL FLUENCIA
    REAL:: AreaE (13000), SAreaE, Area1, Area2, PArea            !Area parciales experimentales, SUMATORIA DE AREA EXPERIMENTAL, AREA BAJO RECTA 1, AREA BAJO RECTA 2, PORCENTAJE ENTRE LAS AREAS
    REAL:: aux, aux2, r,z                                         !AUXILIARES 
    REAL:: RESULTADOS(13000,13000), FinalFY(1,5)
    CHARACTER(LEN=40):: nombre
  
              !AREAS PARCIALES DEL AREA DEBAJO DE LA ENVOLVENTE, SUMATORIA AREA ENVOLVENTE, AREA DEBAJO DE RECTA 1, AREA DEBAJO RECTA 2, % DE DIF ENTRE AREAS

!LECTURA DEL ARCHIVO DE ENTRADA
	OPEN(UNIT=11, FILE='datos.txt', STATUS='OLD', ACTION='READ')
	 READ(11,*) 
	 READ(11,*)nombre
     READ(11,*) 
	 READ(11,*)Intervalo
	 READ(11,*) 
     READ(11,*)NDExp
     READ(11,*)
     DO i=1,NDExp
       READ(11,*)Envolvente(i,1), Envolvente(i,2)
     END DO  
        
    CLOSE(11)
!!! se conviete en 0    
do i=1,13000
    do j=1,13000
        EnvolventeDis(i,j)=0
        RESULTADOS(i,j)=0
        
    end do
end do
       SFY(1,1)=0
       SFY(1,2)=0
        FY(1,1)=0
        FY(1,2)=0
!! se crea la matriz de envolvente discreta
  !se coloca los desplazamientos 

     cont = 2  ! contador de la posición en EnvolventeDis
     conti =0  !contador de orden en vaores de intervalo en intervalo

     DO i=1, NDExp
        if (int(Envolvente (I,1)/Intervalo) > 0) then 
            sumI=0
          Do j=cont, int(Envolvente (I,1)/Intervalo)+cont-1-conti
                conti=conti+1
                EnvolventeDis(j,1)=(conti)*intervalo
               
                sumI=sumI+1
           end do
          cont=cont+sumI
          EnvolventeDis(cont,1)=Envolvente (I,1)
          EnvolventeDis(cont,2)=Envolvente (i,2)
          cont=cont+1
        else
        EnvolventeDis(cont,1)=Envolvente (I,1)
        EnvolventeDis(cont,2)=Envolvente (i,2)
        cont=cont+1
        End if

     End do
!SE COLOCAN LAS CARGAS EN LA DISCRETIZADA
     NDDis=cont-1
     conti=0
     z=1
     do i=2, NDDis
     r=(EnvolventeDis(i,2))
      if (r<z) then
        if(conti>0) then
         aux=(Envolvente(conti+1,2)-Envolvente(conti,2))/(Envolvente(conti+1,1)-Envolvente(conti,1))
         aux2=(EnvolventeDis(i,1)-Envolvente(conti,1))
         EnvolventeDis(i,2)=Envolvente(conti,2)+(aux*aux2)
        
          else
         aux=(Envolvente(conti+1,2))/(Envolvente(conti+1,1))
         aux2=(EnvolventeDis(i,1))
         EnvolventeDis(i,2)=aux*aux2
         end if

      else 
      conti=conti+1
      end if
    end do


    conti=0
    !! SE SACA EL COJUNTO DE RESULTADOS
    DO i=2, NDDis
      SFY(1,1)=EnvolventeDis(i,1)     !desplazamiento
      SFY(1,2)=EnvolventeDis(i,2)     !carga
      FY(1,2)=SFY(1,2)/0.97          !carga fy
      M1=sfy(1,2)/sfy(1,1)            !pendiente recta 1
      Fy(1,1)= fy(1,2)/M1             !deslazamiento fy 
      M2=(ENVOLVENTE(NDExp,2)-FY(1,2))/(ENVOLVENTE(NDExp,1)-FY(1,1))  !PENDIENDE DE LA RECTA 2
      !se crean los datos de las rectas
      IF (FY(1,2)<ENVOLVENTE(NDExp,2) ) then
      SAreaE=0

        do j=1, NDDis
        Recta1(j,1)= EnvolventeDis(j,1)
        Recta1(j,2)= Recta1(j,1)*M1
        Recta2 (j,1)=EnvolventeDis(j,1)
        Recta2 (J,2)= ((Recta2(j,1)-fy(1,1))*M2)+fy(1,2)
        end do

        do j=1, NDDis-1
         !se crea delta d
          DeltaD(j)= EnvolventeDis(j+1,1)-EnvolventeDis(j,1)
          AreaE(j)=((EnvolventeDis(j,2)+EnvolventeDis(j+1,2))/2)*DeltaD(j)
          SAreaE=SAreae+AreaE(j)
         
        end do

        Area1= (fy(1,1)*fy(1,2))/2
        Area2=((fy(1,2)+Recta2(NDDis,2))/2)*(Recta2(NDDis,1)-fy(1,1))
        PArea=(SAreaE-(Area1+Area2))/SAreaE
        conti=conti+1
       RESULTADOS(i-1,1)=FY(1,1)        !DESPLAZAMIENTO EN FY
       RESULTADOS(I-1,2)=abs(PArea)     !porcentaje de aproximación
       RESULTADOS(i-1,3)=FY(1,2)        !carga en FY
       RESULTADOS(i-1,4)=SFY(1,1)       !desplazamiento en 0.75fy
       RESULTADOS(i-1,5)=SFY(1,2)       !carga en 0.75fy



      end if 
      

    END DO
      FinalFY(1,1)=0
      FinalFY(1,2)=10000000000

      do i=1,conti
        if (FinalFY(1,2)>RESULTADOS(i,2)) then 
        FinalFY(1,2)=RESULTADOS(i,2)
        FinalFY(1,1)=RESULTADOS(i,1)
        FinalFY(1,3)=RESULTADOS(i,3)
        FinalFY(1,4)=RESULTADOS(i,4)
        FinalFY(1,5)=RESULTADOS(i,5)
        end if 
      end do 

    

    Print *, conti, RESULTADOS(1,2), RESULTADOS(1,1), FinalFY(1,1), FinalFY (1,2), RESULTADOS(2,2), RESULTADOS(2,1)

    
OPEN(UNIT=11, FILE="RESULTADOS.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '     '
	WRITE(11,*) '                           RESULTADOS   ' , NOMBRE
	WRITE(11,*) "    "
    WRITE(11,*) " LOS DATOS INGRESADOS EXPERIMENTALES FUERON   " 
	  DO i=1,NDExp
       write(11,*) Envolvente(i,1), Envolvente(i,2)
      END DO
     write (11,*) " "
	 WRITE(11,*) "   LOS RESULTADOS SON: " 
	

     write (11,*) '       dfy            %area             Cfy           d0.75fy             C0.75fy   '
      DO i=1,conti
       write (11,*)  RESULTADOS(i,2), RESULTADOS(i,1),  RESULTADOS(i,3),  RESULTADOS(i,4),  RESULTADOS(i,5)
      end do
     write (11,*) " "
     Write (11,*) " DESP FINAL           %AREA               Cfy           d0.75fy             C0.75fy     #datos"
     write (11,*)  FinalFY(1,1), FinalFY (1,2),              FinalFY (1,3)  ,FinalFY (1,4),FinalFY (1,5), conti


CLOSE(11)   

END PROGRAM PRIESTLEY
   