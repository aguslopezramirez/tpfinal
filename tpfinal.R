rm(list = ls())
require("here")
require("ggplot2")
require("maps")

archivo<-here("SYNOP.dat")

#leo los datos de cada estacion por un lado y los datos de cada variable por otro para ponerlos "lindos" y juntarlos

data.estaciones<-read.table(archivo, header = FALSE, sep = "|", dec = "-", fill = TRUE, na.strings = "", skip =26,
                            comment.char = "")

data<-read.table(archivo, header = FALSE, sep = "=", fill = TRUE, skip =26, colClasses = c("character","character"),
                 comment.char = "#")

#na.strings para que tome los espacios vacios como NA
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

synops<-synops[rowSums(is.na(synops)) != ncol(synops),]               #uso esta sentencia para eliminar las lineas con NA

#rowSum hace la suma de cada fila
#is.na(synops) me da true para NA y false para cualquier otra cosa
#si una linea me da true true y hago la suma esta me da 2 que es igual a ncol(synops)
#entonces no se queda con esa linea

#me quedo un array con 66 lineas donde tengo en t.presente valores de t,td,p,nubosidad y viento
#y en t.pasado tengo tmax, tmin, pp


#########DATA.ESTACIONES##########
#voy a extraer n° de estacion, localidad, latitud y longitud

estacion<-data.estaciones[rowSums(is.na(data.estaciones)) == 0,]   #con esta sentencia elimino las lineas vacias y las que contengan algun NA



#numero de estacion y localidad

num.est<-substr(estacion$V1,14,18)

localidad<-substr(estacion$V1,21,regexpr("Arg", estacion$V1)-3)


#latitud
#substraigo los datos y los pongo en formato numerico
  
lat<-substr(estacion$V2,2,6)
lat<-as.numeric(sub("-",".",lat))*-1   

#longitud
#hago lo mismo que con la latitud

lon<-substr(estacion$V3,3,7)
lon<-as.numeric(sub("-",".",lon))
lon<-360-lon

###### DECODIFICACION #######
#Ahora voy a obtener los datos de nubosidad, viento, temperatura,
#temperatura de roció y presión en cada estación junto con su decodificacion
#pero primero voy a sacar los datos que no me sirven del data frame synops (numero de estacion, dia y hora, etc)

#en la estacion del aerodromo de formosa tengo que el dato de n° de estacion esta repetido. Lo saco.
synops$t.presente[10]<-sub(" 87162 87162 "," 87162 ",synops$t.presente[10])

#ahora si me quedo solo con los datos que me importan

synops$t.presente<-substr(synops$t.presente,36,100000)

###viento 

viento<-substr(synops$t.presente,3,6)

#decodificacion -- Nddff N:nubosidad  dd:direccion  ff:velocidad

dd<-(as.numeric(substr(viento,1,2)))*10  #porque el viento esta en decenas de grados
ff<-as.numeric(substr(viento,3,4))      

#voy a sacar los valores de viento de mi data.frame para que no molesten

synops$t.presente<-substr(synops$t.presente,7,100000)

###temperatura

t<-substr(synops$t.presente, regexpr(" 1", synops$t.presente)+1,regexpr(" 1", synops$t.presente)+5)

#decodificacion -- 1STTT S:signo de la t  TTT: t en grados centigrados y con un decimal

TTT<-(as.numeric(substr(t,3,5)))*0.1  

#aca le estoy diciendo que guarde en t los datos de temperatura substrayendolos y convirtiendolos en numericos
#porque los extrae como caracter
#regexpr busca en el synop el patron "espacio1" y me devuelve la posicion en el que lo encontro

###temperatura de rocio

td<-substr(synops$t.presente, regexpr(" 2", synops$t.presente)+1,regexpr(" 2", synops$t.presente)+5)

#decodificacion -- 2STdTdTd  S:signo  TdTdTd: td en grados centigrados con un decimal

S<-as.numeric(substr(td,2,2))    #dos estaciones tienen td negativas
S[S==1]<-(-1)
S[S==0]<-1
Td<-(as.numeric(substr(td,3,5)))*0.1*S

###presion a nivel de la estacion

p<-substr(synops$t.presente, regexpr(" 3", synops$t.presente)+1,regexpr(" 3", synops$t.presente)+5)

#decodificacion -- 3PPPP  PPPP: p en hectopascales sin el mil y con un decimal

P<-(as.numeric(substr(p,2,5)))*0.1
P[which(P <100)]<-P[which(P < 100)]+1000


###nubosidad

nub<-substr(synops$t.presente, regexpr(" 8", synops$t.presente)+1,regexpr(" 8", synops$t.presente)+5)
nub[which(nchar(nub) != 5)]<-NA    #algunas estaciones no tienen datos de nubosidad

#decodificacion -- 8NClCmCh  N:cantidad en octavos  Cl,Cm y Ch tipos de nubes

N<-substr(nub,2,2)


Cl<-as.numeric(substr(nub,3,3))
Cl[which(Cl == 0)]<-"No hay estratocúmulos, estratos, cúmulos o cumulonimbus"
Cl[which(Cl == 1)]<-"Cúmulos con aspecto plano, o cúmulos por mal tiempo, antes o despues de las precipitaciones"
Cl[which(Cl == 2)]<-"Cúmulos de extensión vertical fuerte o moderada con protuberancias tipo torres, acompañados o no por otros cúmulos o estratocúmulos"
Cl[which(Cl == 3)]<-"Cumulonimbus cuya parte superior no tiene perfil puntiagudo y que no son fibrosos ni con forma de yunque"
Cl[which(Cl == 4)]<-"Estratocúmulos formados por el desarrollo de cúmulos"
Cl[which(Cl == 5)]<-"Estratocúmulos que no surgen del desarrollo de cúmulos"
Cl[which(Cl == 7)]<-"Estratos ''fractus'' de mal tiempo"
Cl[which(Cl == 8)]<-"Otros cúmulos y estratocúmulos formados por el desarrollo de cúmulos"

Cm<-as.numeric(substr(nub,4,4))
Cm[which(Cm == 0)]<-"No hay altocúmulos, altoestratos o nimboestratos"
Cm[which(Cm == 1)]<-"Altoestartos en gran parte semitransparentes"
Cm[which(Cm == 3)]<-"Altoscúmulos en gran parte semitransparente"
Cm[which(Cm == 5)]<-"Altocúmulos en bandas semitransparentes, o altocúmulos en una o más láminas semitransparentes u opacas"
Cm[which(Cm == 6)]<-"Altocúmulos resultantes del desarrollo de cúmulos o cumulonimbos"
Cm[which(Cm == 7)]<-"Altocúmulos en dos o más láminas, generalmente opacas en algunos lugares"

Ch<-as.numeric(substr(nub,5,5))
Ch[which(Ch == 0)]<-"No hay cirros, cirrocúmulos ni cirroestratos"
Ch[which(Ch == 1)]<-"Cirros en forma de filamentos o hebras"
Ch[which(Ch == 2)]<-"Cirros densos como manchas que parecen ser restos de la parte superior de un cumulonimbo"
Ch[which(Ch == 3)]<-"Cirros con forma de yunque"
Ch[which(Ch == 6)]<-"Cirros en forma de bandas que convergen hacia dos puntos opuestos del horizonte"
Ch[which(Ch == 8)]<-"Cirroestratos que no cubren la bóveda celeste"
  
#No se encuentran datos de temperatura maxima y minima en las 12hs previas y de precipitacion en las 24hs previas
#para ninguna estacion.


####PARTE B####

#Pongo todas las variables en un data frame

decod<-data.frame(Id = num.est, Latitud = lat, Longitud = lon, Direccion = dd, Velocidad = ff, Temperatura = TTT, TemperaturaRocio = Td, Presion = P, Nubosidad = N, Baja = Cl, Media = Cm, Alta = Ch , stringsAsFactors = FALSE)
decod[is.na(decod)] <- -999.99

file.create("Decodificacion.txt")

write.table(decod, file = "Decodificacion.txt", sep = " ", dec = ".", quote = FALSE, row.names = FALSE)


####PARTE C

cant.estaciones<-length(num.est)
faltantes <- vector("numeric", length = 5)

for(f in 1:5) {
  faltantes[f]<-length(which(decod[[f+4]] == -999.99))
}

print(paste("Se procesaron",cant.estaciones,"estaciones en total y se encontraron",faltantes[1],"datos faltantes de viento,",faltantes[2],"datos faltantes de temperatura,", faltantes[3], "datos faltantes de temperatura de rocio,", faltantes[4], "datos faltantes de pesion y", faltantes[5], "datos faltantes de nubosidad."))


####PARTE D

mapa<-map_data("world2", region = "Argentina")
num<-1:length(localidad)
df<-data.frame(latitudes = lat, longitudes = lon, num = num, localidad = paste(num,localidad), stringsAsFactors = FALSE)

m <- ggplot() +
  theme_bw() + 
  geom_polygon(data = mapa, aes(x=long, y=lat, group =group),color="black", fill="grey") +
  geom_point(data = df, aes(x=longitudes, y=latitudes, fill = localidad), color="purple", size=2.5) +
  geom_text(data = df, aes(longitudes, latitudes, label= num), size=4)  +
  ggtitle("Posición de las estaciones procesadas") +
  xlab("Longitud") +
  ylab("Latitud")

 
ggsave("posicion_estaciones.png", width = 17, height = 10, dpi = 300)
