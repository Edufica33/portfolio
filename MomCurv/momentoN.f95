PROGRAM Programa_MomentoCurvatura

	!Elaborado por Campos, Ramon
	!OBJETIVO: obtener los puntos para la gráfica de momento-curvatura de una sección rectangular con dos lechos de acero
	!UNIVERSIDAD AUTÓNOMA DE YUCATÁN.
	!CAMPUS DECIENCIAS EXACTAS E INGENIERÍAS.
	!FACULTAD DE INGENIERÍA.
	!MAESTRÍA EN INGENIERÍA.
	!OPCIÓN ESTRUCTURAS.

IMPLICIT NONE

!Definición de variables
	!datos de la sección
	REAL:: b,h,r,Ig
		!Base, altura, recubrimiento, inercia gruesa

	!datos de los materiales
	
	REAL::fpc,fc,Es,fs,ft,Ec,n, EPSIcu, EPSIcuCon 
		!f'c, esfuerzo en el concreto, mod. de elasticidad del acero, 
		!resistencia a tensión por flexión del concreto, modulo de elasticidad del concreto, relación modal, def. ultima concreto, def. ultima concreto confinado. 
	REAL:: k,RHOs,fyh,Vsh,Vcc,Sh, diametroh, bpk ,hpk, Zm, EPSIcero, EPSIceroCon
		 !factor k, relación entre volúmenes, fluencia del acero transversal, Vol. de refuerzo trans 
		 !Vol. de concreto confinado, separación entre estribos, Area de estribos
		 !largo de los estribos, alto de los estribos, pendiente de la recta del modelo, deformación en la resistencia máxima del concreto no confinado
	CHARACTER(len=50):: TipoAcero  !tipo de acero que se usará
	REAL:: AsE(20,3) ,As(20,2), Ast, Ag
	 	!AsE: matriz de entrada donde primera columna es el diámetro de la barra, segunda es el número de barras por lecho y tercero la profundidad
		!As: matriz que engloba el área  en la primera columna y la profundidad en la segunda columna (máximo 20 lechos)
	INTEGER :: NL !número de lechos de acero
	!datos a calcular de la sección
	REAL:: Ctr,Itr, Mcr,Mcr2, EPSIt, PHItr, Icr, PHIcr, Ccr, EPSIj, EPSIex
		!centroide en antes del agrietamiento del concreto, Inercia antes del agrietamiento, Monmento en el agrietamiento, def. e
		!curvatura antes del agrietamiento, inercia agrietada, curvatura agrietada, centroide agrietado. 
	REAL:: EPSIs,EPSIc , EPSIce, MP, rho, fy, EPSIsu
	 !def. en el acero, def. en el concreto, porcentaje de acero
	 

	!Variables auxiliares
	INTEGER:: i, cont, cont2
    REAL:: sumCcr1, noCon, SUMIcr
	CHARACTER(len=50):: mensaje (2500), nombre
	REAl:: PHIn, Cn

	!variables para el metodo de disecciones
	REAL:: Fcn1, Fcn2, Fcn3,a, z, maxI, Fsc, Fst, PM, Fncex, Fncj, P, EN1, Msc, Mst
	REAL:: x, y
	REAL:: FibrasCon(1000,6), NF, DF, contPF, Cf, hfrac, MC(2500,4), Asn(20,5), Mcc, Fcc, Mn, PHIb, Cm
    REAL:: hfrac2, rc
	INTEGER:: bandera, 	contPHI, NMaxPHI
	CHARACTER (len=50):: Criterio

	!Nombres de las funciones
    REAL:: fckp, EPSIkp, fsm, Ftn, CT, TT, Cn, tol
    
!LECTURA DEL ARCHIVO DE ENTRADA

	OPEN(UNIT=11, FILE='datos.txt', STATUS='OLD', ACTION='READ')
	 READ(11,*) 
	 READ(11,*) 
	 READ(11,*)nombre
     READ(11,*) 
	 READ(11,*)b,h,r, P
	 READ(11,*) 
     READ(11,*)fpc, NL, TipoAcero
	READ(11,*) 
     READ(11,*)fyh,Sh,diametroh
	 READ(11,*)
	 do i=1,NL
     	READ(11,*) AsE(i,1),  AsE(i,2),  AsE(i,3)   !diametro, numero de barras , profundidad
	 end do
	
  
        
    CLOSE(11)

Print*, b,h,r, fpc, TipoAcero
!Impresión en CMD
	do i=1,NL
     	Print*, AsE(i,1),  AsE(i,2),  AsE(i,3)
	end do

!Se crea la matriz As
 do i=1,NL
    As(i,1)= (3.14159265*((AsE(i,1)/2)**2))* AsE(i,2)  !area de acero por lecho
	As(i,2)= AsE(i,3)  !profundidad del lecho
 end do
!hipótesis 
 !Cálculos preliminares    
	!se corrigen las profundiades del primer y último lecho
		As(1,2)=r+(AsE(1,1)/2)   !la pronfundidad del primer lecho es igual al rec. libre más la mitad del diámetro de las barras de ese lecho
		As(NL,2)=h-(r+(AsE(NL,1)/2)) !la profundidad del último lecho es igual a la altura menos el rec. libre sumado con el radio de las barras de ese lecho
	
        Ag=b*h
        Ast=0
        do i=1, NL
          Ast=Ast+As(i,1)
        end do
                  
        ft=2*SQRT(fpc)
 		Ec=11000*SQRT(fpc)  !11236*SQRT(fpc)
 		Es=2100000
		n=Es/Ec
        rho=Ast/Ag
		rc= r-(diametroh/2) !la distancia de los extremos a los limites del concreto confinado es el rec. libre menos el radio de los estribos
	!se asignan los valores del acero
    if (TipoAcero== "G42") then 
        fy=4200     !esfuerzo nominal a tensión
		EPSIsu=0.1295
    end if 
    if (TipoAcero== "G42RB") then 
        fy=4593     
		EPSIsu=0.1295
    end if 
    if (TipoAcero== "G42T") then 
        fy=5219     
		EPSIsu=0.175
    end if 
    if (TipoAcero== "G56") then 
        fy=5600     
		EPSIsu=0.15
    end if 
    if (TipoAcero== "G56T") then 
        fy=5887.9     
		EPSIsu=0.15
    end if 
 !modelos constitutivos
	!concreto (Kent y Park Modificado)
      bpk=b-(2*rc) !base del area de concreto confinada. se considera al radio de los estriboss
	  hpk=h-(2*rc)  !altura del area de concreto confinada
	  Vsh=(2*hpk+2*bpk)*(3.14159265*(diametroh/2)**2) !volumen de acero confinante
	  Vcc=bpk*hpk*Sh		!volumen del concreto confinado
	  RHOs=Vsh/Vcc
	  K=1+RHOs*(fyh/fpc)     !k para el concreto confinado. Para el no confinado k=1
      EPSIcero=0.002			!deformación a la carga máxima de concreto no confinado
      EPSIceroCon=0.002*k		!defeormación a la carga máxima del concreto confinado
	  Zm=0.5/(((3+0.028423*fpc)/(14.2116*fpc-1000))+((3*RHOs*SQRT(hpk/Sh))/4)-EPSIceroCon)  !pendiente de la recta descendente
	  EPSIcu=0.004 				!deformación última del concreto no confinado
	  EPSIcuCon=0.004+0.9*RHOs*(fyh/3059)  !! deformación última concreto confinado (Scott, Park y Priestley).
	 
!probando que funcione los modelos constitutivos
	noCon=1
		EPSIc=0.000182
		
	fc=fckp(noCon,fpc,EPSIc,zm,EPSIcuCon)

		EPSIs=0.0075
	fs=fsm(TipoAcero,EPSIs,Es)

!PASO 1. Calcular momento y curvatura antes del agrietamiento
	!Se calcula el centroide transformado
		a=0.00    !limite inferior de las posibles C
		z= h   !límite superior de las posibles C
		cont=0	
		maxI=1000  !número máximo de iteraciones
		tol=0.001	   !tolerancia  en porcentaje entre valores 	
		Cn=fpm(a,z)  !la C se vuelve la media los límites 
		!se aplican las fórmulas para obtener el equilibrio a partir del centroide
		do 
				if ( Cn==h ) then!pregunta si el elemento está totalmente a compresión
					Ctr=0     !no existe un crentroide de agrietamiento porque el concreto no se agrieta a tensión
					Mcr=0     !no existe en ese caso
					PHItr=0
					exit
				end if
			
				!se calcula la curvatura antes del agrietamiento
					EPSIt= Ft/Ec ! def. en el extremo inf. se entra al modelo const./se trata de concreto no confinado
					PHItr=EPSIt/(h-Cn) !curvatura antes del agrietamiento 
				!se calculas las deformaciones en los puntos de interés en el concreto y sus respectivos esfuerzos
					EPSIex= PHItr*Cn !deformación en el extremo superior
					EPSIj= PHItr*(Cn-r)  !def. al centroide del estribo superior, punto donde se cambia de concreto no confinado a confinado
				!se calculan las fuerzas 
					!fuerzas de compresión de concreto
						Fncex=fckp(noCon,fpc,EPSIex,zm, EPSIcuCon) !esfuerzo del concreto no conf. en el extremo
						Fncj=fckp(noCon,fpc,EPSIj,zm,EPSIcuCon)   !esfuerzo del concreto no conf. en la junta con  el confi.
						Fcn1=(((Fncex+Fncj)*r)/2)*b !trapecio de esfuerzos del concreto nconf de arriba
						Fcn2=((fckp(noCon,fpc,EPSIj,zm, EPSIcuCon )*(Cn-r))/2)*r*2  !concreto no conf a los lados del concreto confinado 
						Fcn3=  ((fckp(k,fpc,EPSIj,zm, EPSIcuCon )*(Cn-r))/2)*(b-(2*r)) !concreto confinado
					!fuerzas de compresión y tensión en el acero
						Fsc=0
						fst=0
						do i=1, NL
							if ( As(i,2)<Cn ) then !se pregunta si está en la parte de compresión
								Fsc=Fsc+ (As(i,1)*fsm(TipoAcero,((Cn-As(i,2))*PHItr),Es))
							else 
								Fst=Fst+ (As(i,1)*fsm(TipoAcero,(((As(i,2)-Cn)*PHItr)),Es))
							end if

						end do
					!fuerzas de tensión en el concreto
					if (Cn<h) then  !si pregunta si hay tensión en la sección
						Ftn= (ft*(h-Cn)/2)*b
					else 
						ftn=0
					end if
					
					!se saca la sumatoria de fuerzas
						CT=Fcn1+Fcn2+Fcn3+fsc
						TT=Fst+Ftn+P
						
					cont=cont+1
	print *, Cn, CT, TT
				
					!se revisa que se haya cumplido la tolerancia
					
						if ( ABS((CT-TT)/CT)< tol) then
							Ctr=Cn
							exit 
						end if
					!se revisa el numero de iteracion
						if(cont==maxI) then
							Ctr=Cn
							exit
						end if
					!si no se cumplieron las anteriores se prepara la siguiente iteración
				
						if ( CT>TT ) then
									z=Cn
									Cn=fpm(a,z)
									!print*, ' com > ten' ,a, z
								
						else
									a=Cn
									Cn=fpm(a,z)	
									!print*, ' ten > com ' ,a, z
						end if
		end do		!fin del do para encntrar Cn
	!Momento antes del agrietamiento y después del agrietamiento
	!si existe tensión se calcula el momento de agrietamiento
	if ( Ctr > 0 ) then
			Msc=0
			Mst=0
			!se calculo los momentos en los lechos de acero
			do i=1, NL
				if ( As(i,2)<Cn ) then !se pregunta si está en la parte de compresión
					Msc=Msc+(As(i,1)*fsm(TipoAcero,((Ctr-As(i,2))*PHItr),Es))*(Ctr-As(i,2))  !fuerza por el brazo en C
				else 
					Mst=Mst+(As(i,1)*fsm(TipoAcero,(((As(i,2)-Cn)*PHItr)),Es))*(As(i,2)-Ctr)
				end if

			end do
			EN1=((Fncex+2*Fncj)*r)/(3*(Fncex+Fncj))  !distancia del extremo al centroide del trapecio de fuerzas Fcn1
		
			MP=(P*((h/2)-Ctr))
		
		Mcr= Fcn1*(Ctr-EN1)+Fcn2*(2*(Ctr-r)/3)+Fcn3*(2*(Ctr-r)/3)+Msc+Mst+(Ftn*(2*(h-Ctr)/3))+MP
	end if


!PASO 2. Calcular la curvatura después del agrietamiento	
	
		!Se calcula el centroide transformado
		a=0.000   !limite inferior de las posibles C
		z= h	   !límite superior de las posibles C
		cont=0		!cuantas iteraciones para hallar C
		maxI=1000 !número máxim0o de iteraciones
		tol=0.001	   !tolerancia  en porcentaje entre valores 	
		Cn=fpm(a,z)  !la C se vuelve la media los límites 
		!se aplican las fórmulas para obtener el equilibrio a partir del centroide
		do 
				if ( Ctr==0 ) then!pregunta si el elemento está totalmente a compresión
					Ccr=0     !no existe un crentroide de agrietamiento porque el concreto no se agrieta a tensión
					PHIcr=0
					exit
				end if
				!se calcula la curvatura antes del agrietamiento
					!EPSIt se calculó en el punto 1
					PHIcr=EPSIt/(h-Cn) !curvatura antes del agrietamiento 
				!se calculas las deformaciones en los puntos de interés en el concreto y sus respectivos esfuerzos
					EPSIex= PHIcr*Cn !deformación en el extremo superior
					EPSIj= PHIcr*(Cn-r)  !def. al centroide del estribo superior, punto donde se cambia de concreto no confinado a confinado
				!se calculan las fuerzas 
					!fuerzas de compresión de concreto
						Fncex=fckp(noCon,fpc,EPSIex,zm,EPSIcuCon ) !esfuerzo del concreto no conf. en el extremo
						Fncj=fckp(noCon,fpc,EPSIj,zm, EPSIcuCon )   !esfuerzo del concreto no conf. en la junta con  el confi.
						Fcn1=(((Fncex+Fncj)*r)/2)*b !trapecio de esfuerzos del concreto nconf de arriba
						Fcn2=((fckp(noCon,fpc,EPSIj,zm, EPSIcuCon )*(Cn-r))/2)*r*2  !concreto no conf a los lados del concreto confinado 
						Fcn3= ((fckp(k,fpc,EPSIj,zm, EPSIcuCon )*(Cn-r))/2)*(b-(2*r)) !concreto confinado
					!print *, Cn, Fncex, Fncj
					!print*, " "
					!fuerzas de compresión y tensión en el acero
						Fsc=0
						fst=0
						do i=1, NL
							if ( As(i,2)<Cn ) then !se pregunta si está en la parte de compresión
								Fsc=Fsc+ (As(i,1)*fsm(TipoAcero,((Cn-As(i,2))*PHItr),Es))  !area por el esfuerzo con esa def.
							else 
								Fst=Fst+ (As(i,1)*fsm(TipoAcero,(((As(i,2)-Cn)*PHItr)),Es)) !area por el esfuerzo con esa def.
							end if

						end do
					!fuerzas de tensión en el concreto =0
					!se saca la sumatoria de fuerzas
					
						
						CT=Fcn1+Fcn2+Fcn3+fsc
						TT=Fst+P

					

					cont=cont+1
					!print *,'Cn ', Cn, CT, TT, Fcn1,Fcn2, fsc, fs
				
					!se revisa que se haya cumplido la tolerancia
						if (ABS((CT-TT)/CT)< tol) then
							Ccr=Cn
							exit 
						end if
					!se revisa el número de iteracion
						if(cont==maxI) then
							Ccr=Cn
							exit
						end if
					!si no se cumplieron las anteriores se prepara la siguiente iteración
				
						if ( CT>TT ) then
									z=Cn
									Cn=fpm(a,z)
									!print*, ' com > ten' ,a, z
								
						else
									a=Cn
									Cn=fpm(a,z)	
									!print*, ' ten > com ' ,a, z
						end if
		end do
	!Momento antes del agrietamiento y después del agrietamiento
	if ( Ccr > 0 ) then
			Msc=0
			Mst=0
			!se calculo los momentos en los lechos de acero
			do i=1, NL
				if ( As(i,2)<Ccr ) then !se pregunta si está en la parte de compresión
					Msc=Msc+ (As(i,1)*fsm(TipoAcero,((Ccr-As(i,2))*PHItr),Es))*(Ccr-As(i,2))
				else 
					Mst=Mst+As(i,1)*Es*((As(i,2)-Ccr)*PHIcr)*(As(i,2)-Ccr)
				end if

			end do
			print*, Fncex, Fncj
			
	!inercia agrietada
		!Se calcula la inercia agrietada
		sumIcr=0
			do i=1, NL
				if ( As(i,2)<Ccr ) then !se pregunta si está en la parte de compresión
					sumIcr=sumIcr+As(i,1)*(n)*((Ccr-As(i,2))**2)
				else 
					sumIcr=sumIcr+As(i,1)*(n)*((As(i,2)-Ccr)**2)
				end if
			end do
			
		Icr=((b*(Ccr**3))/3)+sumIcr
	
	!Momento después del agrietamiento
			!se calculó en el paso 1.
			!print*, Icr, Mcr, Ec, n
	!Curvatura después del agrietamiento 
		PHIcr=Mcr/(Ec*Icr)
	end if  !fin del if para saber si se hay Icr

					

!Paso 3. Proponer una curvatura mayor que la curvatura después del agrietamiento
	if ( Ccr > 0 ) then
		PHIn= PHIcr
		PHIb=0.000001
	else 
		PHIn=0
		PHIb=0.00000001
	end if
	contPHI= 0
	Mn=0
	bandera=0
	NMaxPHI=2500
    hfrac2= 0 !se inicia el cero el valor final de la altura de fractura 
	!inicia el ciclo para asumir distintas phis

	do 		
			if (bandera == 1 ) then  !se sale si se produce un criterio de falla y no se puede obtener una C
				exit
			end if
			if ( contPHI== NMaxPHI ) then   !se sale si se producen muchos ciclos
				exit
			end if
			contPHI=contPHI+1
			if (Ccr==0) then
				if (Phin>0.000001) then
					PHIb=0.0000001
				end if 
			end if 
			PHIn=PHIn+PHIb

			!PASO 4. Asumir una profundidad del eje neutro “c” 
				a=0.000   !limite inferior de las posibles C
				z= 4.5*h	   !límite superior de las posibles C
				cont2=0	
				maxI=500 !número máximo de iteraciones
				tol=0.00010	   !tolerancia  en porcentaje entre valores 	
				Cn=fpm(a,z)  !la C se vuelve la media los límites 

			!inicia el ciclo para encontrar la C de esa phi con la que se alcanza el equiliibrio
			do
				if ( Cn==h ) then
					Cn=1.1*h
					
				end if
				
				Criterio="No_falla"   !se resetea para cada C para que solo aplique para la ultima C 
				bandera=0      !se resetea para que solo tome lo que pasa con la ultima C que es la de equilibrio
				hfrac=0  !se resetea cual es la altura fractura para cada C, altura del concreto s
				mensaje(contPHI)= ' no_fluye '  !si no ha fluido el acero, no se escribe nada
				!PASO 5. Calcular la deformación unitaria del concreto
				
					!se calculas las deformaciones en los puntos de interés en el concreto y sus respectivos esfuerzos
					
				!paso 6. Revisar las deformaciones en la zona de compresión
	


				!paso 7. Discretizar la zona de compresión de la sección transversal en fibras.  Considerar zonas de concreto confinado y no confinado 
					NF=1000  !número de fibras
					if (Cn>h) then
						DF=h/NF   !altura de fibra, se limita a toda la sección, la cual toda está a compresión.
					else	
						DF=Cn/NF   !aquí no toda la sección está a compresión.
					end if
				!PASO 8. Calcular la profundidad del centroide de cada fibra a compresión.
					contPF=0
					do i=1 ,NF
						FibrasCon(i,1)=contPF+((DF/2))!se obtiene la profundidad de cada fibra 
						contPf=(DF*i)      !esto hará que el proximo este DF/2 más abajo de la parte mas baja de la fibra anterior
					end do
	!print *,' profundidad', FibrasCon(1,1), FibrasCon(2,1)
				!Paso 9. Calcular la deformación del concreto ∈_c en el centroide de cada fibra a compresión
					do i=1 ,NF
						FibrasCon(i,2)=PHIn*(abs(Cn-FibrasCon(i,1)))  !se obtiene la def. en el centroide de cada fibra 
					end do
	!print *,' deformacion',FibrasCon(1,2), FibrasCon(2,2)
				
				!se revisa que no haya fibras confinadas con esfuerzo =0
				!Paso 10. Calcular el esfuerzo del concreto f_c en el centroide de cada fibra a compresión
					!solo se tomará el concreto para compresión, dado que después de sismos previos las columnas ya están agrietadas
					!recordar que el programa ya identifica que cuando Cn>=H todo el elemento está a compresión y se divide las fibras en H, cuando no se dividen en Cn
					do i=1 ,NF  
						if (FibrasCon(i,1)<(rc)) then !pregunta si está en el primer rectángulo no confinado
							if	(FibrasCon(i,2) .LE. (EPSIcu))  then !pregunta si no se ha fracturado el concreto no conf.
								FibrasCon(i,3)= fckp(noCon,fpc,FibrasCon(i,2),zm, EPSIcuCon)  !si no está fracturado esfuerzo no confinado en esa fibra
							else  !si está fracturado(en este criterio de falla ninguno puede estar fracturado)
								FibrasCon(i,3)=0   !no hay esfuerzo si está fracturado
								hfrac=(FibrasCon(i,1)) + DF/2  !la altura de esa fibra se convierte en la altura que ya se fracturó
							end if
							FibrasCon(i,4)= 0						 !nunca habrá esfuerzo confinado en esa fibra
						
						else   !si está debajo del rectángulo totalmente no confinado

							if (FibrasCon(i,1)>(h-rc)) then !pregunta si esta en el segundo rectángulo no confinado
								!está en el segundo rectángulo de no conf.
								if	(FibrasCon(i,2) .LE. (EPSIcu))  then !pregunta si no se ha fracturado el concreto no conf.
									FibrasCon(i,3)= fckp(noCon,fpc,FibrasCon(i,2),zm, EPSIcuCon)  !si no esta fracturado esfuerzo no confinado en esa fibra
								else  !si está fracturado. en este criterio de falla ninguno puede estar fracturado
									FibrasCon(i,3)=0   !no hay esfuerzo si está fracturado
									hfrac=(FibrasCon(i,1)) + DF/2  !la altura de esa fibra se convierte en la altura que ya se fracturó
								end if
								FibrasCon(i,4)= 0						 !nunca habrá esfuerzo confinado en esa fibra	

							else !está en la zona con concreto confinado la fibra está entre rc y H-rc

								!para la parte del concreto no confinado
								
								if	(FibrasCon(i,2) .LE. (EPSIcu))  then !pregunta si no se ha fracturado el concreto no conf.
									FibrasCon(i,3)= fckp(noCon,fpc,FibrasCon(i,2),zm, EPSIcuCon)  !si no esta fracturado esfuerzo no confinado en esa fibra
								else  !si está fracturado
									FibrasCon(i,3)=0   !no hay esfuerzo si está fracturado
									hfrac=(FibrasCon(i,1)) + DF/2  !la altura de esa fibra se convierte en la altura que ya se fracturó, tomado como la parte inferior
								end if

								!para la parte del concreto confinado
								if (  FibrasCon(i,2)<(EPSIcuCon)) then  !pregunta si se fractura el estribo
									FibrasCon(i,4)= fckp(k,fpc,FibrasCon(i,2),zm, EPSIcuCon)	!esfuerzo confinado
								else 
									FibrasCon(i,4)=0				!el concreto confinado está aplastado
									bandera=1              !se reporta que la sección ya falló
									Criterio = ' aplastamiento_del_concreto_confinado '
									exit
								end if !fin de la colocación de esfuerzos.

							end if !fin del if que pregunta si está en el segundo rectangulo no conf

						end if  !fin del if que pregunta si está debajo del primer rectángulo no confinado
				
					end do !end del do del paso 10
						
				!Paso 11. Calcular la fuerza de compresión de cada fibra multiplicando el esfuerzo f_c  por el área de cada fibra	
					do i=1 ,NF
						if (FibrasCon(i,1)<(rc)) then !pregunta si está sobre la parte de confinamiento 
							FibrasCon(i,5)=  FibrasCon(i,3)*b*DF  !esfuerzo no confi *  base* altura de la fibra 
						else 
							if (FibrasCon(i,1)>(h-rc)) then !pregunta si esta en el segundo rectángulo no confinado
								FibrasCon(i,5)=  FibrasCon(i,3)*b*DF !está en el segundo rectágulo no confinado
							else  !está en la zona con concreto confinado la fibra está entre rc y H-rc
								FibrasCon (i,5)= FibrasCon(i,3)*(2*rc)*DF  !esfuerzo no confi *  base* altura de la fibra
								FibrasCon (i,5)= FibrasCon(i,5)+(FibrasCon(i,4)*bpk*DF)  !fuerza no confi+ (esfuerzo confinado *  base* altura de la fibra)
							end if					
						end if 
					end do					
	
					!Se saca la sumatoria de fuerzas de compresión
						Fcc=0
						do i=1, NF	
							Fcc=Fcc+FibrasCon(i,5)
						end do
	                    !print *,'  sumatoria comp',Fcc					
		!Paso 12. Calcular la profundidad de cada lecho del acero de refuerzo por flexión
			!este paso es un dato de entrada que se encuentra en la primera columna de la matriz As()
			do i=1, NL
				Asn(i,1)=As(i,2)   !profundidad del lecho
				Asn(i,4)=As(i,1)   !área de acero del lecho
			end do
		!Paso 13. Calcular la deformación unitaria del acero ∈_s para cada lecho
			do i=1, NL
				if ( Asn(i,1) < Cn ) then !se pregunta si está en la parte de compresión
					Asn(i,2)= (Abs(Cn-Asn(i,1)))*PHIn
				else !si está en la parte de tensión
					Asn(i,2)= (Asn(i,1)-Cn)* abs(PHIn)  !check
                    if (Asn(i,2) .GT. ((fy+1)/Es) ) then
						mensaje(contPHI)= ' Fluencia_acero_tension ' !solo se reporta que ya fluyo el acero
                    end if
                    
				end if
               
			end do

		!Paso 14. Calcular el esfuerzo f_s en cada lecho del acero (tensión o compresión).  
			do i=1, NL
				Asn(i,3)= fsm(TipoAcero,Asn(i,2),Es)  !se entra al modelo consti del tipo de acero con la def del lecho
					 
                    if ( Asn(i,3)==0 ) then  
						bandera=1                       !se reporta que la sección ya falló
						Criterio = 'Fractura_del_acero_del_lecho_inferior ' 
						exit
						!print *,criterio , Cn
					
					end if					
			end do

		!print *, CT-TT,'   As1 ',Asn(1,2), '   As2 ', Asn(2,2)

		!Paso 15. Calcular la fuerza en cada lecho del acero (tensión o compresión). 
			!fuerzas de compresión y tensión en el acero
				Fsc=0
				Fst=0
				do i=1, NL
					if ( Asn(i,1)<Cn ) then !se pregunta si está en la parte de compresión
						Fsc=Fsc+(Asn(i,3)*Asn(i,4))!se suman las fuerzas de compresión
					else 
						Fst=Fst+(Asn(i,3)*Asn(i,4))	!se suman las fuerzas de tensión
					end if
				end do
	
				!Paso 16. se revisa el equilibrio

					!fuerzas de tensión en el concreto =0
					!se saca la sumatoria de fuerzas
						CT=Fcc+Fsc
						TT=Fst+P

					
		!print *,' equilibrio ',CT , TT	
						
					cont2=cont2+1
				!	print *, Cn, CT, TT}
					
				
					!se revisa que se haya cumplido la tolerancia
						if ( ABS((CT-TT)/CT)< tol) then
							exit !salida del do para encontrar C
						end if
					!se revisa el número de iteración
						if(cont2==maxI) then
							exit  !salida del do para encontrar C
						end if
					!si no se cumplieron las anteriores se prepara la siguiente iteración
				
						if ( CT>TT ) then
									z=Cn
									Cn=fpm(a,z)
									!print*, ' com > ten' ,a, z
								
						else
									a=Cn
									Cn=fpm(a,z)	
									!print*, ' ten > com ' ,a, z
						end if
			end do   !fin del do para hallar c
print *, 'fin del ciclo Cn, Phin:' ,PHIn, contPHI,'           Cn ', Cn, cont2
print*, " "
Print *,  "dif", CT-TT,"  hfrac ", hfrac2
		!MOMENTO 
		!se pregunta si la C sale de la sección
			Cm=Cn
			!Momento generados por el acero
				Msc=0
				Mst=0
				!se calcula los momentos en los lechos de acero
				do i=1, NL
					if ( Asn(i,1)<Cm ) then !se pregunta si está en la parte de compresión
						Msc=Msc+ Asn(i,3)*Asn(i,4)*(Cm-Asn(i,1)) !el momento es igual a la fuerza en la fibra por el brazo de palanca
						!Msc=Msc+ Asn(i,3)*Asn(i,4)*(As(i,1))
					else !si está en la parte de tensión
						Mst=Mst+Asn(i,3)*Asn(i,4)*(Asn(i,1)-Cm)
						!Mst=Mst+Asn(i,3)*Asn(i,4)*(Asn(i,1))
					end if
				end do
			!Momento generados por el concreto
				Mcc=0
				do i=1,NF
					Mcc=Mcc+FibrasCon(i,5)*(Cm-FibrasCon(i,1))  !la fuerza en esa fibra por el brazo de palanca al centroide
					!Mcc=Mcc+FibrasCon(i,5)*(FibrasCon(i,1))  
				end do
		!se saca el momento para la phin
			
			Mn=Mcc+Msc+Mst+P*((h/2)-Cm)
			!Mn=Mcc+Msc+Mst+P*((h/2))
			!print*, P*((h/2)-Cm), Mcc, Mst, Msc
        if (hfrac2<hfrac) then
            hfrac2=hfrac
        end if 
!print*, '    Momento ',Mn
	!se asignan los valores a la matriz de momento curvatura					
	 	MC(contPHI,1)=PHIn
	 	MC(contPHI,2)=Mn	
		MC(contPHI,3)=Cn
		MC(contPHI,4)=hfrac2

			

	end do !termina el ciclo de las phi asumidas

	
	
		


!Impresión de resultados
 OPEN(UNIT=11, FILE="RESULTADOS.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '                       Nombre_de_la_prueba: ' , NOMBRE 
	WRITE(11,*) "    "
     Write (11,*) " Datos_de_entrada"
     write (11,*) ' b ', b,' h ', h, ' r ', r
	 write (11,*) ' fpc ', fpc, "carga_axial", P, "k", k
     write(11,*)  "tipo_acero: ", TipoAcero, ' rho ', rho
	 write (11,*) "     Datos del acero " 
	 write (11,*) " diámetro      núm._de_barras         profundidad "
	do i=1, NL
	  write (11,*) AsE(i,1), AsE(i,2), AsE (i,3)
	end do
	Write (11,*) ' '
    Write (11,*) 'datos_estribos'
     Write (11,*) 'fy_est: ', Fyh, ' sep_estr: ', Sh, ' diam_estr:', diametroh
    Write (11,*) ' '
     Write (11,*) "Criterios_de_iteración"
     write (11,*) 'iter_máx_para_C', maxI,'tolerancia_para_C', tol, ' núm.de_fibras: ', NF
	 write (11,*) "                Resultados           "
	 write (11,*) 'Ctr ', Ctr, ' PHItr ',  PHItr, ' Mcr ', Mcr 
	 write (11,*) " "
	 write (11,*)' Ccr ', Ccr,' PHIcr ' , PHIcr
	 write (11,*) " "
	 write (11,*)' Criterio_de_falla: ', Criterio
	 WRITE (11,*)'Cm_de_recubrimiento_perdidos: ' ,hfrac2
	 write (11,*) "	curvatura	Momento		eje_Neutro		cm_perdidos_de_concreto   Fluye? "
     write (11,*)
     write (11,*)0, 0, 0, 0
     write (11,*)PHItr,Mcr, Ctr, 0,0
     write (11,*) PHIcr, Mcr,Ccr,0,0
	do i=1, contPHI
	 write (11,*) MC(i,1), MC(i,2), MC(i,3), MC(i,4), mensaje(i)
	end do
	! write (11,*)
	! do i=1, 100000
	!	write (11,*) 0.00001*i, fckp(k,fpc,0.00001*i,zm, EPSIcuCon )
	!end do
 CLOSE(11)   
contains 
	real function fpm(a,b)
		real::a,b, fpm
		fpm= (a+b)/2.0
	end function
		
END PROGRAM Programa_MomentoCurvatura

!función para calcular fc del concreto según el modelo de Kent y Park modificado.    
 function fckp(k1,fpc1, EPSIc1,Zm1,EPSIcuCon1)
 IMPLICIT NONE
 REAL :: fckp 					 !Variable dummy
 REAL :: k1,fpc1, EPSIc1,Zm1, EPSIcuCon1      !!variables locales

	if (EPSIc1>0) then 
		if (EPSIc1 .LE. 0.002*K1) then !pregunta si está en la zona ascendente
   	 		fckp=k1*fpc1*(((2*EPSIc1)/(0.002*k1))-(EPSIc1/(0.002*k1))**2)
		else    !si esta en la zona de descarga
			if ( K1==1) then  !significa que se trata de concreto no confinado 
				if ( EPSIc1 .LE. 0.004 ) then !se pregunta si ya se ha fracturado el concreto no confinado
					fckp=k1*fpc1*(1-(Zm1*(EPSIc1-0.002*k1))) ! si no está está en la curva descendente
				else !si está fracturado el no conf.
					fckp=0 !no produce esfuerzos
				end if
			else ! se trata de concreto confinado
				if (EPSIc1 .LE. EPSIcuCon1) then   !se pregunta si está fracturado el concreto confinado 
					fckp=k1*fpc1*(1-(Zm1*(EPSIc1-0.002*k1)))
						if (fckp<0.2*k1*fpc1) then !el esfeurzo producido por no confinado no puedo ser menor a este
							fckp=0.2*k1*fpc1
						end if
				else !si está fracturado el conf.
					fckp=0 !no produce esfuerzos
				end if
			end if
		end if 		
    else 
		fckp=0
	end if
 END function fckp

!función para calcular los esfuerzos en el acero dependiendo del tipo de acero y la deformación segun los modelos constitutivos
 function fsm (tipo,EPSIs1,Es1)  
 IMPLICIT NONe
 REAL :: fsm
 REAL :: fy1, EPSIs1, Es1,fu1, EPSIsh1, EPSIsu1, P1      !!variables locales
 CHARACTER(len=50):: Tipo
	if (EPSIs1>0) then 
	!acero grado 42 nominal con def. max. de de Rodríguez y Botero
		if (tipo=='G42') then
		 fy1=4200     !esfuerzo nominal a tensión
		 EPSIsu1=0.1295
			if (EPSIs1 .LE. (fy1/Es1)) then !pregunta si es menor a la def. de fluencia
   	 			fsm=Es1*EPSIs1    !está en el rango lineal
			else !sino está en el rango lineal
				if (EPSIs1 .LE. EPSIsu1) then  !pregunta si es menor a la def de falla
   	 			fsm=fy1     !está en la mesesta 
				else     !si es mayor a la de falla
					fsm=0  !ya falló
				end if
			end if 	
		end if		
	!acero grado 42 según el artículo de Rodriguez y Botero
		if (tipo=='G42RB') then
			!datos promedios
			fy1=4593 
			fu1=7463  
			EPSIsh1=0.0074
			EPSIsu1=0.1295
			P1=3.418
			if (EPSIs1>(fy1/Es1)) then        !pregunta si ya llegó a la fluencia
   	 			if (EPSIs1 >(EPSIsh1)) then    !pregunta si ya llegó a la def de endurecimiento
					if ( EPSIs1>EPSIsu1 ) then!pregunta si ya llegó a la falla
						fsm=0	!ya falló 
					else
						fsm=fu1+(fy1-fu1)*((EPSIsu1-EPSIs1)/(EPSIsu1-EPSIsh1))**P1  !está en la curva de endurecimiento
					end if
				else
					fsm=fy1  !está en la meseta plástica
				end if
			else 
				fsm=Es1*EPSIs1	!está en el rango elástico lineal
			end if 		
		end if!fin del G42 Rod. y bot. 
	
	!acero grado 42 tarea, según está en el archivo de las inst. de la tarea
		if (tipo=='G42T') then
			!datos obtenidos
			fy1=5219
			EPSIsh1=0.0172
			EPSIsu1=0.175
			if (EPSIs1>(fy1/Es1)) then        !pregunta si ya llegó a la fluencia
   	 			if (EPSIs1 >(EPSIsh1)) then    !pregunta si ya llegó a la def de endurecimiento
					if ( EPSIs1>EPSIsu1 ) then!pregunta si ya llegó a la falla
						fsm=0	!ya falló 
					else
						fsm=745488*EPSIs1**3 -316980*EPSIs1**2 + 45700*EPSIs1+4613.7!está en la curva de endurecimiento
					end if
				else
					fsm=fy1  !está en la meseta plástica
				end if
			else 
				fsm=Es1*EPSIs1	!está en el rango elástico lineal
			end if 		
		end if


	!acero grado 56 nominal 
		if (tipo=='G56') then
		fy1=5600
		EPSIsu1=0.15
			if (EPSIs1 .LE. (fy1/Es1)) then !pregunta si es menor a la def. de fluencia
   	 			fsm=Es1*EPSIs1    !está en el rango lineal
			else !sino está en el rango lineal
				if (EPSIs1 .LE. EPSIsu1) then  !pregunta si es menor a la def de falla
   	 			fsm=fy1     !está en la mesesta 
				else     !si es mayor a la de falla
					fsm=0  !ya falló
				end if
			end if 	
		end if		

	!acero grado 56 tarea, según está en el archivo de las inst. de la tarea
		if (tipo=='G56T') then
			!datos obtenidos
			fy1=5887.9 
			EPSIsh1=0.0148
			EPSIsu1=0.15
			if (EPSIs1>(fy1/Es1)) then        !pregunta si ya llegó a la fluencia
   	 			if (EPSIs1 >(EPSIsh1)) then    !pregunta si ya llegó a la def de endurecimiento
					if ( EPSIs1>EPSIsu1 ) then!pregunta si ya llegó a la falla
						fsm=0	!ya falló 
					else
						fsm=1000000*EPSIs1**3 - 443329*EPSIs1**2 + 53694*EPSIs1+5337.8!está en la curva de endurecimiento
					end if
				else
					fsm=fy1  !está en la meseta plástica
				end if
			else 
				fsm=Es1*EPSIs1	!está en el rango elástico lineal
			end if 		
		end if

	!acero grado 60-modelo bilineal con endurecimiento por deformación lineal con datos de Ramírez (2021)
		if (tipo=='G60') then
			fy1=8086
			EPSIsu1=0.12
			if (EPSIs1 .LE. (fy1/Es1)) then !pregunta si es menor a la def. de fluencia
   	 			fsm=Es1*EPSIs1    !está en el rango lineal
			else !sino está en el rango lineal
				if (EPSIs1 .LE. EPSIsu1) then  !pregunta si es menor a la def de falla
   	 			fsm=(((8530-fy1)/(0.12-(fy1/Es1)))*EPSIs1)+fy1     !está en la mesesta con ligera pendiente
				else     !si es mayor a la de falla
					fsm=0  !ya falló
				end if
			end if 		
		end if

	
    else 
		fsm=0        !se introdujo una deformación negativa
	end if

 	END function fsm

!FUNCIONES PARA CALCULAR DEFORMACIONES 

 !función para calcular EPSIc del concreto según el modelo de Kent y Park modificado.    
  function EPSIkp(k1,fpc1, fc1,Zm1)
  IMPLICIT NONE
  REAL :: EPSIkp 					 !Variable dummy
  REAL :: k1,fc1, fpc1,Zm1      !!variables locales
	if (fc1>0) then 
		if (fc1<fpc1*K1) then 
   	 		EPSIkp=0.002*k1*(fpc1+SQRT((fpc1**2)-((fc1*fpc1)/k1)))
		else 
			EPSIkp=((1-fc1/(k1*fpc1))/Zm1)+0.002*k1  
				
		end if 		
    else 
		EPSIkp=0
	end if
  END function EPSIkp


