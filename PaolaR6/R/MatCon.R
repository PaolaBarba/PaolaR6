#' @title Confusion matrix
#' @description Using the confusion matrix, various indices are calculated.
#' @param values Confusion matrix
#' @param ID Identifier (optional)
#' @param Date System or user-provided date (optional)
#' @return Object of class MatCon #y mas
#' @export MatCon
#' @importFrom R6 R6Class
#' @examples
#' C = matrix( c(5, 0, 1, 0,4,0,0,0,3), nrow=3,  ncol=3)
#' mc2 <- MatCon$new (C,ID=5,Date="27-10-2023")


# Ahora con R6
MatCon <- R6Class("MatCon",
                             public = list(
                               #inicializa la matriz de confución. debe de añadirse una matriz
                               values = NULL,
                               #inicializa nombre
                               ID = NULL,
                               #inicializa rango
                               nk = NULL,
                               #inicializando Fecha
                               Date = NULL,
                               #inicializa suma filas
                               sumfil=NULL,
                               #suma columnas
                               sumcol=NULL,


                               #aqui añado todos los parametros

                               initialize = function(values,ID=NULL,Date=NULL) {
                                 #El usuario debe dar la matriz de confusion
                                 self$values<-values

                                 #Es opcional que identifique su matriz.
                                 #Si añade este valor pues se le da un ID personalizado a la MC
                                 #sino se le dará un número aleatorio que puede ir de 1 a 1000
                                 #ID="Paola" o ID=5
                                 if(!is.null(ID)){
                                   self$ID <- ID
                                 }else{self$ID<-sample(c(1:1000),1,replace=FALSE)}
                                 #Si no se añade fecha (Date=2710, Date="27-10", Date="27/10")
                                 #En ese caso se tomara la fecha del sistema
                                 if(!is.null(Date)){
                                   self$Date<-Date
                                 }else{self$Date <- Sys.Date()}

                                 #Valores para chequear la MC
                                 #rango de la matriz
                                 #nk<-nrow(self$valores)

                                 nfilas <- nrow(self$values)
                                 ncolumnas <- ncol(self$values)
                                 #suma de los elementos de la filas
                                 self$sumfil<-apply(self$values,1,sum) #definido para algunas funciones
                                 #suma de los elementos de la columna
                                 self$sumcol<-apply(self$values,2,sum) #definido para algunas funciones

                                 #cat(paste0("filas ", nfilas, ".\n"))  PARA INFORME
                                 #cat(paste0("columnas ", ncolumnas, ".\n"))
                                 error1<- FALSE
                                 error2<- FALSE
                                 error3<- FALSE
                                 error4<- FALSE
                                 error5<- FALSE
                                 error6<- FALSE

                                 if((nfilas != ncolumnas)) {
                                   error1<- TRUE #error matriz no cuadra
                                 }
                                 #Con la comprobacion del error1 ya se ha comprobado el error2
                             #    if((nfilas != nk)) {
                            #       error2<- TRUE #rg distinto a nfilas
                             #    }
                                 for (i in 1:nfilas){
                                   for (j in 1:ncolumnas){if(self$values[i,j]<0) {
                                     error3<-TRUE
                                     }
                                   } #error elementos menores que 0
                                 }
                                 if(sum(self$values)==0 ){
                                   error4<-TRUE #error si la suma de los valores de la matriz es 0
                                 }
                                 if(sum(apply(self$values,1,sum))==0 ){
                                   error5<-TRUE #suma filas es 0
                                 }

                                 if(sum(apply(self$values,2,sum))==0 ){
                                   error6<-TRUE #suma columnas es 0
                                 }
                                 if ((error1 == TRUE) || (error2==TRUE) || (error3 == TRUE) || (error4 == TRUE) || (error5==TRUE) || (error6 == TRUE)) {
                                   #self$er=TRUE
                                   warning("Errores de tipo 1, 2, 3,4,5 o 6") #chequeo hecho
                                   stop()
                                 }
                               },



                               #' @description Overall accuracy for a particular classified image/map is then calculated by dividing the sum of the entries that form the major diagonal (i.e., the number of correct classifications) by the total number of samples taken.
                               #' @param ... (ignored).
                               #' @description
                               #' The mathematical expression is:
                               #'
                               #' \deqn{
                               #' oa = \frac{\sum_{i=1}^{n} x_{ii}}{\sum_{i, j=1}^{n} x_{ij}}
                               #' }
                               #'
                               #' Where:
                               #' \enumerate{
                               #'   \item `oa`: overall accuracy.
                               #'   \item `x_ii`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' This represents a mathematical expression with a fraction.
                               #'
                               #' @references Story, M., & Congalton, R. G. (1986). Accuracy assessment: a user’s perspective. Photogrammetric Engineering and remote sensing, 52(3), 397-399.
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$oa()

                               oa = function(...) {
                                 indice <- sum(diag(self$values))/sum(self$values)
                                 return(indice)
                               },

                               #' @description Determines whether a value is decimal or not.
                               #' @param ... (ignored).
                               #' @return TRUE or FALSE
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$dec()

                               dec = function(...){
                                 for (i in 1:length(self$values)){
                                   dec = if (round(self$values[i],0) == self$values[i]) {FALSE} else {TRUE}
                                 }
                                 return(dec)
                               },


                               #' @description  The accuracy from the point of view of a map user, not the map maker.
                               #' @description
                               #' The mathematical expression is:
                               #' \deqn{
                               #' ua=\frac{x_{ii}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `ua`: user accuracy.
                               #'   \item `x_ii`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param ... (ignored).
                               #' @references Story, M., & Congalton, R. G. (1986). Accuracy assessment: a user’s perspective. Photogrammetric Engineering and remote sensing, 52(3), 397-399.
                               #' @return vector of values with the user's accuracy indexes of all classes
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A,ID=1,Date="30/10/2023")
                               #' p$ua()

                               ua = function(...){
                                 n <- sqrt(length(self$values))
                                 ua <- rep(0,n)

                                 for (i in 1:n){

                                   ua[i] <- self$values[i,i] / self$sumfil[i] #Ind Exactitud Usuario para todas las clases a la vez
                                 }

                                 return(ua)
                               },

                               #' @description The accuracy from the point of view of a map user, not the map maker.
                               #' @description
                               #'  \deqn{
                               #' ua_{i}=\frac{x_{ii}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `ua_i`: user accuracy.
                               #'   \item `x_ii`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param i User class to evaluate
                               #' @return Class i user accuracy index
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$uai(2)

                               uai=function(i){

                                 uai = self$values[i,i] / self$sumfil[i]

                                 return(uai)
                               },

                               #' @description  The map accuracy from the point of view of the map maker (the producer).
                               #' @description
                               #'  \deqn{
                               #' pa_{i}=\frac{x_{jj}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `pa_i`: producer accuracy.
                               #'   \item `x_jj`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param i Producer class to evaluate
                               #' @return Class i producer accuracy index
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$pai(1)

                               pai = function(i){

                                 pai = self$values[i,i] / self$sumcol[i]

                                 return(pai)
                               },


                               #' @description  The map accuracy from the point of view of the map maker (the producer).
                               #' @description
                               #'  \deqn{
                               #' pa=\frac{x_{jj}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `pa`: producer accuracy.
                               #'   \item `x_jj`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param ... (ignored).
                               #' @return Vector of values with the producer's accuracy indexes of all classes
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$pa()

                               pa = function (...){
                                 n <- sqrt(length(self$values))
                                 pa <- rep(0,n)
                                 for(i in 1:n){
                                   pa[i] <- self$values[i,i] / self$sumcol[i]
                                 }
                                 return(pa)
                               },

                            #' @description  Average of the accuracy from the point of view of a map user, not the map maker and the map accuracy from the point of view of the map maker (the producer).
                            #' @description
                            #'  \deqn{
                            #' aup=\frac{uai+pai}{2}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `aup`: Average of user's and producer's accuracy.
                            #'   \item `uai`: user accuracy
                            #'   \item `pai`: producer accuracy.
                            #' }
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$aup(2)

                            #Average of user's and producer's accuracy
                            aup=function(i){

                              aup = (self$uai(i) + self$pai(i))/2

                              return(aup)
                            }


                             ),

                             private = list(
                               nUsos = NULL
                             ),
                             active = list(

                             )

)

