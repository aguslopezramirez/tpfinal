rm(list = ls())
require("here")

archivo<-here("SYNOP.dat")

#leo los datos de cada estacion por un lado y los datos de cada variable por otro para ponerlos "lindos" y juntarlos

data.estaciones<-read.table(archivo, header = FALSE, sep = "|", fill = TRUE, na.strings = "", skip =26,
                 comment.char = "")

data<-read.table(archivo, header = FALSE, sep = "=", fill = TRUE, skip =26, colClasses = c("character","character"),
                 comment.char = "#")

#header=FALSE para que no considere que la primer fila es el encabezado
#fill=TRUE para que complete con espacios en blanco
#colClasses para que considere que las columnas son de clase character
#skip para que salte las primeras 26 lineas
#comment.char = "" porque quiero que me muestre los comentarios


#########DATA##########

#con un ciclo tengo que separar los datos presentes de los datos del pasado (los que siguen el 333)
#antes creo dos vectores vacios

t.presente<-as.vector(NA, mode = "character")

t.pasado<-as.vector(NA, mode = "character")

#creo el ciclo

for (i in 1:109) {
  if (substr(data$V1[i],1,1)==2) {t.presente[i]<-data$V1[i]}
  if (substr(data$V1[i],20,20)==3) {t.pasado[i-1]<-substr(data$V1[i],20,1000000)}
}

#hago un data frame con los dos vectores para que me queden todos los datos
#de una misma estacion en la misma linea

synops<-data.frame(t.presente,t.pasado, stringsAsFactors = FALSE)     #stringAsFactors porque no quiero que convierta los vectores en factores

synops<-synops[rowSums(is.na(synops)) != ncol(synops),]          #uso esta sentencia para eliminar las lineas con NA

#rowSum hace la suma de cada fila
#i.na(synops) me da true para NA y false para cualquier otra cosa
#si una linea me da true true y hago la suma esta me da 2 que es igual a ncol(synops)
#entonces no se queda con esa linea

#me quedo un array con 66 lineas donde tengo en t.presente valores de t,td,p,nubosidad y viento
#y en t.pasado tengo tmax, tmin, pp


#########DATA.ESTACIONES##########

estacion<-data.estaciones[rowSums(is.na(data.estaciones)) == 0,]   #con esta sentencia elimino las lineas vacias y las que contengan algun NA






