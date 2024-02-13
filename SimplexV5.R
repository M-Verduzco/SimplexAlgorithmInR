library(matlib)



#Función axuliar que maneja el rango de error computacional
simplificarVectoraCeros <- function(input){
  for (i in 1:length(input)){
    if(abs(input[i])<=0.0001){
      input[i]<- 0
    }
  }
  input
  

}

simplificarMatrizaCeros <- function(input){
  for (i in 1:nrow(input)){
    for (j in 1:ncol(input)){
      if(abs(input[i,j]<=0.0001)){
        input[i,j]<-0
      }
    }
  }
  input
}




# vectorDePrueba <- c(1, 2, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001)
# res <- simplificarVectoraCeros(vectorDePrueba)
# res
# 




#   Funcion que genera la primera base con los datos iniciales

procesar_datos_iniciales <- function(b, c){
  
  xB <- names(c[(length(c)-length(b)+1):length(c)])
  xB
  
}
#     Para la prueba
# c <- c(3, 5, 0, 0, 0)
# names(c) <- c('x1', 'x2', 's1', 's2', 's3')
# b<-c(4,12,18)
# res <- procesar_datos_iniciales(b,c)
# res








#   Funcion que genera la segunda seccion con la nueva base dada
generar_vectores_NBase <- function(nuevaBase, A, c){

  xB <- nuevaBase
  B <- matrix(A[, (nuevaBase)], nrow = length(nuevaBase), ncol = length(nuevaBase), byrow = FALSE)
  B_inverse <- solve(B)
  cb <- c[nuevaBase]
  names(cb) <- nuevaBase
  res <- list(xB, B, B_inverse, cb)
  res
  
}
  #     Para la prueba
  # nuevaBase <- c('s1', 'x2', 's3')
  # A <- matrix(c(1,0,1,0,0,0,2,0,1,0,3,2,0,0,1), byrow = TRUE, nrow = 3)
  # colnames(A) <- c('x1', 'x2', 's1', 's2', 's3')
  # c <- c(3, 5, 0, 0, 0)
  # names(c) <- c('x1', 'x2', 's1', 's2', 's3')
  # 
  # res <- generar_vectores_NBase(nuevaBase, A, c)
  # res[1]
  # res[2]
  # res[3]
  # res[4]










#   Funcion que evalua genera la seccion tres generada a partir de los datos de la seccion dos y su respectiva base
evaluarBase <- function(cB, B, A, B_inverse, b, c){
   
  zrow <- cB%*%B_inverse%*%A - c 
  ci <- B_inverse%*%A
  zsol <- cB%*%B_inverse%*%b
  bj <- B_inverse%*%b
  res3 <- list(zrow, ci, zsol, bj)
  res3
  
}
#    Para la prueba
#    A <- matrix(c(1,0,1,0,0,0,2,0,1,0,3,2,0,0,1), byrow = TRUE, nrow = 3)
#    cB <- c(0,0,0)
#    B <- matrix(c(1,0,0,0,1,0,0,0,1), ncol=3, nrow=3, byrow = TRUE)
#    B_inverse <- solve(B)
#    B_inverse
#    b <- c(4,12,18)
#    c <- c(3, 5, 0, 0, 0)
#    res <- evaluarBase(cB, B, A, B_inverse, b, c)
#    res[1]
#    res[2]
#    res[3]
#    res[4]
   
   








todosPositivosOCero <- function(vectorInput){
  flag = TRUE
  for (i in 1:(length(vectorInput))){
    if(vectorInput[i]<0){
      flag = FALSE
    }
  }
  flag
  
}
# para la prueba
# vectorUno <- c(2, 3, 6, 2, 0, 5)
# vectorDos <- c(2, 3, 4, 5, 6, 7, -6)
# resUno <- todosPositivosOCero(vectorUno)
# resDos <- todosPositivosOCero(vectorDos)
# resUno
# resDos










todosNegativosOCero <- function(vectorInput){
  flag = TRUE
  for (i in 1:(length(vectorInput))){
    if(vectorInput[i]>0){
      flag = FALSE
    }
  }
  flag
}
# para la prueba
 # vectorUno <- c(-2, -3, -6, -2, 0, -5)
 # vectorDos <- c(-2, -3, -4, -5, -6, -7, 6)
 # resUno <- todosNegativosOCero(vectorUno)
 # resDos <- todosNegativosOCero(vectorDos)
 # resUno
 # resDos









#   Funcion auxiliar que nos dice nos pregunta si llegamos al Optimo (MAXIMIZACION)
esOptimo_MAX <- function(bj, zrow){
  if(todosPositivosOCero(bj) && todosPositivosOCero(zrow))
    res<-TRUE
  else
    res<-FALSE
  res
}
# pruebas:
# bUno <- c(10, 4, 4)
# zRowUno <- c(3, 4, 5, 0, 0, 0)
# bDos <- c(10, 4, 4)
# zRowDos <- c(3, -6, 5, 0, 0, 0)
# resUno <- esOptimo_MAX(bUno, zRowUno)
# resDos <- esOptimo_MAX(bDos, zRowDos)
# resUno
# resDos










#   Funcion auxiliar que nos dice nos pregunta si llegamos al Optimo (MINIMIZACION)
esOptimo_MIN <- function(bj, zrow){
  if(todosPositivosOCero(bj) && todosNegativosOCero(zrow))
    res<- TRUE
  else
    res<- FALSE
  res
}
#  pruebas:
#  bUno <- c(10, 4, 4)
#  zRowUno <- c(3, 4, 5, 0, 0, 0)
#  bDos <- c(10, 4, 4)
#  zRowDos <- c(-3, -6, -5, 0, 0, 0)
#  resUno <- esOptimo_MIN(bUno, zRowUno)
#  resDos <- esOptimo_MIN(bDos, zRowDos)
#  resUno
#  resDos









#   Simplex Primal Max
simplexPrimalMax <- function(bj, zrow, ci){
  
  nombresBase <- names(bj)
  vectorEntrada <- names(which(zrow == min(zrow)))[1]
  vectorAuxiliar <- bj/ci[,vectorEntrada ]
  colIndexAux <- which(ci[, vectorEntrada]>0)
  vectorSalida <- names(which(vectorAuxiliar == min(vectorAuxiliar)))
  nombresBase[which(nombresBase==vectorSalida)] = vectorEntrada
  nombresBase
  
}
#   pruebas:
  # bj <- c(4, 12, 18)
  # names(bj) <- c('s1', 's2', 's3')
  # zrow <- c(-3, -5, 0, 0, 0)
  # names(zrow) <- c('x1', 'x2', 's1', 's2', 's3')
  # ci <- matrix(c(1, 0, 1, 0, 0, 0, 2, 0, 1, 0, 3, 2, 0, 0, 1), byrow=TRUE, ncol=5, nrow = 3)
  # rownames(ci) <- c('s1', 's2', 's3')
  # colnames(ci) <- c('x1', 'x2', 's1', 's2', 's3')
  # res <- simplexPrimalMax(bj, zrow, ci)
  # res
  # 





#   Simplex Primal Min
simplexPrimalMin <- function(bj, zrow, ci){
  
  nombresBase <- names(bj)
  vectorEntrada <- names(which(zrow == max(zrow)))[1]
  vectorAuxiliar <- bj/ci[,vectorEntrada ]
  colIndexAux <- which(ci[, vectorEntrada]>0)
  vectorSalida <- names(which(vectorAuxiliar == min(vectorAuxiliar)))
  nombresBase[which(nombresBase==vectorSalida)] = vectorEntrada
  nombresBase

}
#   pruebas:
  # bj <- c(4, 12, 18)
  # names(bj) <- c('s1', 's2', 's3')
  # zrow <- c(5, 1, 0, 0, 0)
  # names(zrow) <- c('x1', 'x2', 's1', 's2', 's3')
  # ci <- matrix(c(1, 0, 1, 0, 0, 0, 2, 0, 1, 0, 3, 2, 0, 0, 1), byrow=TRUE, ncol=5, nrow = 3)
  # rownames(ci) <- c('s1', 's2', 's3')
  # colnames(ci) <- c('x1', 'x2', 's1', 's2', 's3')
  # res <- simplexPrimalMin(bj, zrow, ci)
  # res









#   Simplex Dual 
simplexDual <- function(bj, zrow, ci){
  
  nombresBase <- names(bj)
  vectorSalida <- names(which(bj==min(bj)))
  zRowIndexesAux <- which(zrow<0)
  vectorAuxiliar <- abs(zrow[zRowIndexesAux]/ci[which(bj==min(bj)), zRowIndexesAux])
  vectorEntrada <- names(which(vectorAuxiliar == min(vectorAuxiliar)))
  nombresBase[which(nombresBase==vectorSalida)] = vectorEntrada
  nombresBase

}
# Para probarlo:
 # bj = c(-6, -8)
 # nombresBase = c('e1', 'e2')
 # names(bj) = nombresBase
 # ci<- matrix(c(-5, -4, 1, 0, -10, -4, 0, 1), byrow=TRUE, ncol=4, nrow = 2)
 # colnames(ci) = c('x1', 'x2', 'e1', 'e2')
 # rownames(ci) = nombresBase
 # zrow <- c(-60,-40, 0, 0)
 # names(zrow) = c('x1', 'x2', 'e1', 'e2')
 # res <- simplexDual(bj, zrow, ci)
 # res









#   Funcion auxiliar que se pregunta si utilizaremos Simplex Primal en TRUE y si no, utilizaremos DUAL
esPrimal <- function(bj){
  if(todosPositivosOCero(bj))
    res<-TRUE
  else
    res<- FALSE
  res
}
#para la prueba: Ojo_ Hay que activar "todosPositivosOCero"
# bjUno <- c(3, 4, -10)
# bjDos <- c(3, 3, 1, 4)
# resUno <- esPrimal(bjUno)
# resDos <- esPrimal(bjDos)
# resUno
# resDos







#   Función extra para establecer una matriz estetica:

funcionRecopiladora<- function(zRow, zSol, ci, bj, xB){
  
  semisolutionMatrix1 <- matrix(zRow, byrow = TRUE, nrow = 1)
  semisolutionMatrix1 <- cbind(semisolutionMatrix1, zSol)
  semisolutionMatrix2 <- cbind(ci, bj)
  #semisolutionMatrix1
  #semisolutionMatrix2
  solutionMatrix <- rbind(semisolutionMatrix1, semisolutionMatrix2)
  #solutionMatrix
  colNamesAux <- append(names(zRow), "sol", 5)
  colnames(solutionMatrix) <-colNamesAux
  rowNamesAux <- append(xB, "b.v.", 0)
  rownames(solutionMatrix) <- rowNamesAux
  solutionMatrix
  
}
#  Para las pruebas
# xB <- c("s1", "s2", "s3")
# zSol <- 0
# zRow <- c(-3, -5, 0, 0, 0)
# names(zRow) <- c('x1', 'x2', 's1', 's2', 's3')
# bj <- c(4, 12, 18)
# names(bj) <- c('s1', 's2', 's3')
# ci <- matrix(c(1, 0, 1, 0, 0, 0, 2, 0, 1, 0, 3, 2, 0, 0, 1), byrow=TRUE, ncol=5, nrow = 3)
# rownames(ci) <- c('s1', 's2', 's3')
# colnames(ci) <- c('x1', 'x2', 's1', 's2', 's3')
# res <- funcionRecopiladora(zRow, zSol, ci, bj, xB)
# res


#*************************************************************************************
#************************F U N C I O N   P R I N C I P A L ***************************
#*************************************************************************************

algoritmoFinal <- function(c, b, A, max, acuracy){
  bandera = FALSE #Variable auxiliar para conocer optimalidad
  SalidaDeEmergencia = FALSE
  i=1 #contador para evitar loops
  xB <- procesar_datos_iniciales(b, c)#Procesar datos iniciales
  
  if(max==TRUE){ #CASO MAXIMIZACIÓN
    
    while(bandera == FALSE && i<=1000 && SalidaDeEmergencia==FALSE){#CICLAMOS con la  bandera y el contador como condiciones
      
      tryCatch({
        lista2 <- generar_vectores_NBase(xB, A, c) #Primera sección de resultados
      }, error = function(e){ SalidaDeEmergencia <<- TRUE})
      if(SalidaDeEmergencia==FALSE){
      
      lista2 <- generar_vectores_NBase(xB, A, c)#Primera sección de resultados
      B <- lista2[2]
      B_inverse <- lista2[3]
      cB <- lista2[4]
      
      lista3 <- evaluarBase(cB[[1]], B[[1]], A, B_inverse[[1]], b, c) #Segunda sección de resultados
      Zrow <- lista3[1]
      Zsol <- lista3[3]
      cij <- lista3[2]
      bj <- lista3[4]
    
      bjAux <- as.vector(bj[[1]]) #Limpieza y transformación de resultados
      names(bjAux) <- xB
      zRowAux <- as.vector(Zrow[[1]])
      names(zRowAux) <- names(c)
      cijAux<- cij[[1]]
      rownames(cijAux) <- xB
      zSolAux <- as.numeric(Zsol[[1]])
      
      if(acuracy==TRUE){ #Caso de precisión computacional
        zRowAux <- simplificarVectoraCeros(zRowAux)
        cijAux <- simplificarMatrizaCeros(cijAux)
      }
      
      if(esOptimo_MAX(bjAux, zRowAux)==FALSE){ #Condicion de optimalidad
        
        if(esPrimal(bj[[1]])==TRUE){ #Caso Primal o Dual (Condición de factibilidad)
          nuevaBase <- simplexPrimalMax(bjAux, zRowAux, cijAux)
        }else{
          nuevaBase <- simplexDual(bjAux, zRowAux, cijAux)
        }
        xB <- nuevaBase #Guardamos la nueva base y aumentamos el contador
        i=i+1;
        
      }else{
        bandera = TRUE; #En el caso óptimo, convertimos la bandera a TRUE
      }
      }
    }
    
    
    
  }else{ #CASO MINIMIZACIÓN
    
    while(bandera == FALSE && i<=1000 && SalidaDeEmergencia==FALSE){#CICLAMOS con la  bandera y el contador como condiciones
      
      tryCatch({
      lista2 <- generar_vectores_NBase(xB, A, c) #Primera sección de resultados
      }, error = function(e){ SalidaDeEmergencia <<- TRUE})
      if(SalidaDeEmergencia==FALSE){
        
      B <- lista2[2]
      B_inverse <- lista2[3]
      cB <- lista2[4]
      
      lista3 <- evaluarBase(cB[[1]], B[[1]], A, B_inverse[[1]], b, c) #Segunda sección de resultados
      Zrow <- lista3[1]
      Zsol <- lista3[3]
      cij <- lista3[2]
      bj <- lista3[4]
      
      bjAux <- as.vector(bj[[1]]) #Limpieza y transformación de resultados
      names(bjAux) <- xB
      zRowAux <- as.vector(Zrow[[1]])
      names(zRowAux) <- names(c)
      cijAux<- cij[[1]]
      rownames(cijAux) <- xB
      zSolAux <- as.numeric(Zsol[[1]])
      
      if(acuracy==TRUE){ #Caso de precisión computacional
        zRowAux <- simplificarVectoraCeros(zRowAux)
        cijAux <- simplificarMatrizaCeros(cijAux)
      }
      
      if(esOptimo_MIN(bjAux, zRowAux)==FALSE){##Condicion de optimalidad
        
        if(esPrimal(bj[[1]])==TRUE){#Caso Primal o Dual (Condición de factibilidad)
          nuevaBase <- simplexPrimalMin(bjAux, zRowAux, cijAux)
        }else{
          nuevaBase <- simplexDual(bjAux, zRowAux, cijAux)
        }
        xB <- nuevaBase #Guardamos la nueva base y aumentamos el contador
        i=i+1;
        
      }else{
        bandera = TRUE #En el caso óptimo, convertimos la bandera a TRUE
      }
    }
    }
  }
  
  if(SalidaDeEmergencia == TRUE){
    matrixRes <- "Tuvimos algún error computacional en las matrices, ¿Ya intentaste activar la precisión?"
  }else{
    if(!bandera){ #Nos preguntamos si encontramos un óptimo
      matrixRes <- "No pudimos encontrar el óptimo"
    }else{
      matrixRes <- funcionRecopiladora(zRowAux, zSolAux, cijAux, bjAux, xB) #Intentamos recopilar los resultados
    }
  }
  
  matrixRes #Regresamos los resultados
}


#*************************************************************************************
#************************M A I N    D E   P R U E B A S ******************************
#*************************************************************************************

#   Informacion que da el usuario:



c1 <- c(3, 5, 0, 0, 0)
c_names1 <- c("x1", "x2", "s1", "s2", "s3")
names(c1) = c_names1
b1 <- c(4, 12, 18)
A1 <- matrix(c(1,0,1,0,0,0,2,0,1,0,3,2,0,0,1), byrow = TRUE, nrow = 3)
colnames(A1)<- c_names1
max1 = TRUE


#   Utilizar el algoritmo para un caso de maximización

miResultado1<-algoritmoFinal(c1, b1, A1, max1, FALSE)
miResultado1











#   Informacion que da el usuario:



c2 <- c(60, 40, 0, 0)
c_names2 <- c("x1", "x2", "e1", "e2")
names(c2) = c_names2
b2 <- c(-6, -8)
A2 <- matrix(c(-5, -4, 1, 0, -10, -4, 0, 1), byrow = TRUE, nrow = 2, ncol = 4)
colnames(A2)<- c_names2
max2 = FALSE


#   Utilizar el algoritmo para un caso de minimización

miResultado1<-algoritmoFinal(c2, b2, A2, max2, FALSE)
miResultado1





c3 <- c(3, 2, 0,0,0)
c_names3 <- c('x1', 'x2', 'e1', 'e2', 's3')
names(c3) <- c_names3
b3 <- c(-3, -6, 3)
A3 <- matrix(c(-3, -1, 1, 0, 0, -4, -3, 0, 1, 0, 1, 1, 0, 0, 1), byrow = TRUE, nrow = 3, ncol = 5)
colnames(A3) <- c_names3
max3 = FALSE

miResultado3<-algoritmoFinal(c3, b3, A3, max3, FALSE)
miResultado3






c3 <- c(3, 2, 0,0,0)
c_names3 <- c('x1', 'x2', 'e1', 'e2', 's3')
names(c3) <- c_names3
b3 <- c(-3, -6, 3)
A3 <- matrix(c(-3, -1, 1, 0, 0, -4, -3, 0, 1, 0, 1, 1, 0, 0, 1), byrow = TRUE, nrow = 3, ncol = 5)
colnames(A3) <- c_names3
max3 = FALSE

miResultado3<-algoritmoFinal(c3, b3, A3, max3, TRUE)
miResultado3









