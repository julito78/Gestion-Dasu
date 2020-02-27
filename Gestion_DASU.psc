Algoritmo Gestion_DASU
	
	opcion Es Caracter;
	posicion Es Entero;
	Dimension posicion[8];
	documento Es Caracter;
	Dimension documento[8]
	pid Es Entero;
	tpbusqueda Es Caracter;
	docum_aux Es Entero;
	Dimension docum_aux[8];
	ind Es Entero;
	ind1 Es Entero;
	ind2 Es Entero;
	texto Es Caracter;
	
	sw_busqueda es logico;
	
	
	posicion[1] <- 5;
	posicion[2] <- 7;
	posicion[3] <- 6;
	posicion[4] <- 4;
	posicion[5] <- 2;
	posicion[6] <- 3;
	posicion[7] <- 1;
	posicion[8] <- 8;
	
	documento[1] <- "Hola, ¿cómo estás?"
	documento[2] <- "Examen UF2178"
	documento[3] <- "Factura cliente FUJITSU"
	documento[4] <- "En un lugar de la Mancha, de cuyo....."
	documento[5] <- "Por favor, no molestar, estamos de examen"
	documento[6] <- "A preguntar al maestro armero"
	documento[7] <- "San Serenin del Monte"
	documento[8] <- "$"
	

	opcion <- Menu()
	Si opcion = 's' o opcion = 'S'  Entonces
		Escribir 'Fin del proceso';
	SiNo
		Segun ConvertirANumero(opcion) Hacer
			1:
			Escribir 'Escribir el valor del PID ..: ' Sin Saltar
			Leer val;
			pid <- val
			VisualizarDocu(posicion,documento,8,pid)
		2:
			sw_busqueda <- Falso
			
			Repetir
				Escribir '              BUSQUEDA DE DOCUMENTO ';
				Escribir '              ===================== ';
				Escribir 'a - Busqueda por número de documento';
				Escribir 'b - Busqueda por palabra en documento (sólo un documento';
				Escribir 'c - Busqueda documentos (todos los documentos)';
				Escribir 'Escribir el tipo de búsqueda ..: ' Sin Saltar
				Leer tpbusqueda;
				
				Si tpbusqueda = 'a' o tpbusqueda = 'b' o tpbusqueda = 'c'  Entonces
					sw_busqueda <- Verdadero
				SiNo
					Escribir '** TIPO OPCION ERRONEA **'  
					Escribir ' '
				Fin Si
			Hasta Que sw_busqueda = Verdadero 
			
			Si tpbusqueda = 'a' Entonces
				Escribir 'Escribir el indice de búsqueda ..: ' Sin Saltar
				Leer ind;
			SiNo
				Escribir 'Escribir la palabra de búsqueda ..: ' Sin Saltar
				Leer palabra;
				ind <- 0
			Fin Si
				
			BuscarDocu(posicion,documento,8,tpbusqueda,ind,palabra,docum_aux)
			
			Si tpbusqueda = 'b' Entonces
				Si ind > 0
					Escribir 'El PID en el que encuentra es :' posicion[ind]
					Escribir 'En el documento ... .' documento[ind] 
				SiNo
					Escribir 'PALABRA NO ENCONTRADO EN LOS DOCUMENTOS '
				FinSi
			SiNo
				Si tpbusqueda = 'c' Entonces 
					Si ind > 0
						ind1 <- 1
						Repetir
							ind2 <- docum_aux[ind1];
							Escribir 'El PID en el que encuentra es .. ' posicion[ind2]
							Escribir 'En el documento ... .' documento[ind2] 
							ind1 <- ind1 + 1
						Hasta Que docum_aux[ind1] = 0 o ind1 > 8
					FinSi
				FinSi
			FinSi	
			
		3:
			Escribir 'Texto a añadir ...: ' Sin Saltar
			Leer texto
			Escribir 'Escribir el valor del PID ..: ' Sin Saltar
			Leer val;
			pid <- val
			EditarDocu(posicion,documento,8,pid,texto)
		4:
			Escribir 'Texto a añadir ...: ' Sin Saltar
			Leer texto
			ind <- 0
			CrearDocu(posicion,documento,8,texto, ind)
			Escribir 'El documento nuevo es el ..: ' posicion[ind]
		5:
			Escribir 'Indicar el valor del PID ..: ' Sin Saltar
			Leer val;
			pid <- val
			BorrarDocu(posicion,documento,8,pid)
		De Otro Modo:
			ImprimirDocu()
			
		Fin Segun
	FinSi
	
FinAlgoritmo

Funcion  seleccion <- Menu ()
	seleccion Es Caracter;
	correcto Es logico;
	correcto <- Verdadero
	
	Repetir
		Escribir '      MENU GESTION DASU';
		Escribir '      =================';
		Escribir '1º Visualizacion de Documento';
		Escribir '2º Búsquda de documento';
		Escribir '3º Edición de documento';
		Escribir '4º Creación de documento';
		Escribir '5º Borrado de documento';
		Escribir '6º Impresión de docuemento';
		Escribir 'Indica la opción a seleccionar de 1 a 6 o s/S para salir ..: ' sin Saltar;
		Leer seleccion
		
		Si seleccion = 's' o seleccion = 'S' Entonces
			correcto <- Verdadero
		SiNo
			Si ConvertirANumero(seleccion) > 0 y ConvertirANumero(seleccion) < 7  Entonces
				correcto <- Verdadero
			SiNo
				correcto <- Falso
				Escribir 'Opción no existe, deve introducir una opción valida (1/6) '
			Fin Si
		Fin Si
	Hasta Que correcto
Fin Funcion

SubProceso VisualizarDocu(posicion Por Referencia,documento Por Referencia,tamanno, val)
	
	Escribir 'val ..: ' val
	
	encontrado es Logico
	ind Es Entero;
	ind <- 1
	encontrado <- falso
	Repetir
		Escribir 'posicion ..: ' posicion[ind]
		Si posicion[ind] = val Entonces
			encontrado <- Verdadero
		SiNo
			ind <- ind + 1
		Fin Si
	Hasta Que encontrado = Verdadero o ind > tamanno
	
	Si encontrado Entonces
		Escribir documento[ind]
	SiNo
		Escribir 'No existe el documento para el valor ..:' val;
	Fin Si
FinSubProceso

subProceso BuscarDocu (posicion Por Referencia,documento Por Referencia, tamanno, tbq, ind por Referencia, plbr, tbla_doc por Referencia)
	palabra_aux Es Caracter;
	palabra_aux2 Es Caracter;
	ind2 Es Entero;
	ind3 Es Entero;
	ind4 Es Entero;
	corte Es logico;
	ind4 <- 1;
	
	corte <- Falso;
	ind2<-1
	
	Segun tbq Hacer
		'a':
			Escribir 'El numero documento buscado es ..: ' posicion[ind]
		'b':
			corte <- Falso;
			ind3 <- 1
			Repetir
				palabra_aux <-	documento[ind3]
				ind2 <- 0
				Escribir 'palabra_aux .. :' palabra_aux
				Repetir
					encontrando<-Falso
					Repetir
						
						Si SubCadena(palabra_aux,ind2,ind2) = ' ' o SubCadena(palabra_aux,ind2,ind2) = ',' Entonces
							corte <- Verdadero;
						SiNo
							palabra_aux2 <-Concatenar(palabra_aux2,SubCadena(palabra_aux,ind2,ind2))
							Escribir 'palabra_aux2 .. :' palabra_aux2
						Fin Si
						ind2 <- ind2 + 1
					Hasta Que corte = Verdadero o ind2 > Longitud(palabra_aux)
					
					Si Mayusculas(palabra_aux2) = Mayusculas(plbr) Entonces
						encontrando <- Verdadero
						ind <- ind3
						Escribir 'encontrando .. ' encontrando
						
					SiNo
						Escribir Mayusculas(palabra_aux2)
						Escribir Mayusculas(plbr)
						corte <- Falso;
						palabra_aux2 <- ''
					Fin Si
				Hasta Que encontrando = Verdadero o ind2 > Longitud(palabra_aux)
				
				
//				Escribir 'palabra_aux2 .. :' palabra_aux2
//				Escribir 'palabra_aux .. ' palabra_aux
				ind3 <- ind3 + 1
			Hasta Que encontrando = Verdadero o ind3 > tamanno
					
		'c':
			corte <- Falso;
			
			Para i<-1 Hasta tamanno Con Paso 1 Hacer
				palabra_aux <-	documento[i]
				ind2 <- 0
				Escribir 'palabra_aux .. :' palabra_aux
				Repetir
					encontrando<-Falso
					Repetir
						//						ind2 <- ind2 + 1
						
						Si SubCadena(palabra_aux,ind2,ind2) = ' ' o SubCadena(palabra_aux,ind2,ind2) = ',' Entonces
							corte <- Verdadero;
						SiNo
							palabra_aux2 <-Concatenar(palabra_aux2,SubCadena(palabra_aux,ind2,ind2))
//							Escribir 'palabra_aux2 .. :' palabra_aux2
						Fin Si
						ind2 <- ind2 + 1
					Hasta Que corte = Verdadero o ind2 > Longitud(palabra_aux)
					
					Si Mayusculas(palabra_aux2) = Mayusculas(plbr) Entonces
						encontrando <- Verdadero
						Escribir 'encontrando .. ' encontrando
						 
					SiNo
						Escribir Mayusculas(palabra_aux2)
						Escribir Longitud(palabra_aux2)
						Escribir Mayusculas(plbr)
						corte <- Falso;
						palabra_aux2 <- ''
					Fin Si
				Hasta Que encontrando = Verdadero o ind2 > Longitud(palabra_aux)
				
				Si encontrando = Verdadero Entonces
					//Control de haber enconrado la palabra en algún documento
					ind <- 99
					tbla_doc[ind4] <- i
					ind4 <- ind4 + 1
					encontrando <- Falso
					 
				Fin Si
				
//				Escribir 'palabra_aux2 .. :' palabra_aux2
//				Escribir 'palabra_aux .. ' palabra_aux
				palabra_aux2 <- ''
			Fin Para
	Fin Segun
FinSubProceso

SubProceso EditarDocu(posicion Por Referencia,documento Por Referencia,tamanno, val,texto)
	
	palabra_aux Es Caracter;
	encontrado es Logico
	ind Es Entero;
	
	ind <- 1
	encontrado <- falso
	
	Repetir
		Si posicion[ind] = val Entonces
			encontrado <- Verdadero
		SiNo
			ind <- ind + 1
		Fin Si
	Hasta Que encontrado = Verdadero o ind > tamanno
	
	Si encontrado Entonces
		palabra_aux <- documento[ind];
		palabra_aux <-Concatenar(palabra_aux,' ')
		palabra_aux <-Concatenar(palabra_aux,texto)
		Escribir 'palabra_aux,texto).. ' palabra_aux
		documento[ind] <- palabra_aux
		Escribir 'documento[ind] .. ' documento[ind]
	SiNo
		Escribir 'No existe el documento aeditar para el valor ..:' val;
	Fin Si
	
FinSubProceso

Subproceso CrearDocu(posicion Por Referencia,documento Por Referencia,tamanno,texto, val Por Referencia)
				 
	encontrado es Logico
	
	ind <- 1
	encontrado <- falso
	
	Repetir
		Si documento[ind] = "$" Entonces
			encontrado <- Verdadero
		SiNo
			ind <- ind + 1
		Fin Si
	Hasta Que encontrado = Verdadero o ind > tamanno
	
	Si encontrado Entonces
		documento[ind] <- texto
		Escribir 'documento[ind] .. ' documento[ind]
		val <- ind
	SiNo
		Escribir 'No hay espacio'; 
	Fin Si
FinSubProceso

SubProceso BorrarDocu(posicion Por Referencia,documento Por Referencia,tamanno, val)
	 
	encontrado es Logico
	ind Es Entero;
	
	ind <- 1
	encontrado <- falso
	
	Repetir
		Si posicion[ind] = val Entonces
			encontrado <- Verdadero
		SiNo
			ind <- ind + 1
		Fin Si
	Hasta Que encontrado = Verdadero o ind > tamanno
	
	Si encontrado Entonces
		documento[ind] <- "$"
		Escribir 'documento[ind] .. ' documento[ind]
	SiNo
		Escribir 'No existe el documento borrar para el valor ..:' val;
	Fin Si
FinSubProceso

Subproceso ImprimirDocu()
	opc_cola Es Entero;
	//Definicion de estructura tipo cola
	Definir MAXCOLA Como Entero;
	MAXCOLA <- 10;
	Definir frente Como Entero;
	Definir final Como Entero;
	Definir numele Como Entero;
	Definir cola Como Caracter;
	Dimension cola[MAXCOLA];
	//Fin definicion de cola
	
	Definir vacia Como Logico;
	Definir llena Como Logico;
	Definir exito Como Logico;
	Definir elemento Como Caracter;
	Definir entrada Como Entero;
	Definir i, f Como Entero;
	numele <- 0;
	frente <- 0;
	final <- 1;
	 
	Repetir
		Escribir " ";
		Escribir '      MENU IMPRESION DASU';
		Escribir '      ===================';
		Escribir "Elija una de las siguientes opciones:";
		Escribir "   1 - Introducir elemento en la cola";
		Escribir "   2 - Sacar elemento de la cola"; 
		Escribir "   3 - Visualizar los elementos de la cola";
		Escribir "   4 - Inicializar la cola";
		Leer opc_cola
		Escribir opc_cola
		Si opc_cola < 0 o opc_cola > 4 Entonces
			Escribir " **Opcion ERRONEA **";
		Fin Si
	Hasta Que opc_cola > 0 y opc_cola < 5
	
	
	Segun opc_cola Hacer
		1:
			Escribir "Introduce elemento: "
			Leer elemento;
			Metercola(cola, frente, final, numele, elemento, exito, MAXCOLA)
			Si exito Entonces
				Escribir "El elemento ", elemento, " esta acolado en la posicion ", numele;
			SiNo
				Escribir "cola LLENA. el elemento ", elemento, " no se ha introducido";
			FinSi
			Escribir "frente = ", frente, "   final = ", final, "   numele = ", numele;
		2:
			Sacarcola(cola, frente, final, numele, elemento, exito)
			Si exito Entonces
				Escribir "El elemento sacado es: ", elemento;
			SiNo
				Escribir "cola VACIA. No existe ningun elemento";
			FinSi
			Escribir "frente = ", frente, "   final = ", final, "   numele = ", numele;
		3:
			Escribir "La cola tiene los siguientes elementos segun entrada y salida"
			f <- frente;
			Escribir "numele = ", numele;
			Para i <- 1 Hasta numele Con Paso 1 Hacer
				Escribir "i = ", i, "  f = ", f;
				Escribir "El elemento ", i, " es ", cola[f];
				f <- f + 1;
			FinPara
		4:
			Inicializarcola(cola, frente, final, numele);
			Escribir "cola inicializada";
	Fin Segun
FinSubProceso

SubProceso Metercola (colam Por Referencia, frentem Por referencia, finalm Por referencia, numelem Por referencia, elem Por Valor, exitom Por Referencia, maximom Por Valor)
	Si ColaLlena(colam, numelem, maximom) Entonces 
		exitom <- Falso;
	SiNo
		Si (finalm = maximom + 1) Entonces
			ReorganizarCola(colam, frentem, finalm, numelem, maximom);
		FinSi
		colam[finalm] <- elem;
		finalm = finalm + 1;
		numelem = numelem + 1;
		exitom <- Verdadero;
	FinSi
FinSubProceso

SubProceso Sacarcola (colasa Por Referencia, frentesa Por referencia, finalsa Por referencia, numelesa Por referencia, elesa Por Referencia, exitosa Por Referencia)
	Si ColaVacia(colasa, frentesa, finalsa, numelesa) Entonces 
		exitosa <- Falso;
	SiNo
		elesa <- colasa[frentesa]
		numelesa = numelesa - 1;
		Si numelesa = 0 Entonces
			Inicializarcola(colasa, frentesa, finalsa, numelesa);
		SiNo
			frentesa <- frentesa + 1;
		FinSi
		exitosa <- Verdadero;
	FinSi
FinSubProceso

SubProceso Inicializarcola (colai Por Referencia, frentei Por referencia, finali Por Referencia, numelei Por Referencia)
	frentei <- 1;
	finali <- 1;
	numelei <- 0;
FinSubProceso

Funcion llena <- ColaLlena (colal, numelel, maximol)
	Si numelel = maximol Entonces
		llena <- Verdadero;
	SiNo
		llena <- Falso;
	FinSi
Fin Funcion

SubProceso ReorganizarCola (colao Por Referencia, frenteo Por referencia, finalo Por referencia, numeleo Por Valor, maximoo Por Valor) 
	Escribir "frente = ", frenteo, "   final = ", finalo, "   numele = ", numeleo;
	Para i <- 1 Hasta numeleo Con Paso 1 Hacer
		colao[i] <- colao[frenteo];
		frenteo = frenteo + 1;
	FinPara
	frenteo <- 1;
	finalo <- numeleo + 1;
	Escribir "frente = ", frenteo, "   final = ", finalo, "   numele = ", numeleo;
FinSubProceso

Funcion vacia <- ColaVacia (colav, frentev, finalv, numelev)
	Si numelev = 0 Entonces
		vacia <- Verdadero;
	SiNo
		vacia <- Falso;
	FinSi
Fin Funcion

