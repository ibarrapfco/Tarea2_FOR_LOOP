#1) Cargue las bases de datos incoporando en cada una de ellas la variable 
#"tamanio", donde indique de que tamaño es la empresa de ese país.(1 pto)



#instalacion de paquetes 
install.packages("tidyverse")
install.packages("csv")

#carga de librerias
library("csv")
library("rvest")
library("tidyverse")

#adress de las bases de datos (sujeto a cambios segun se encuentren)

setwd("~/R/Tarea2")


#PERU
PequenaPeru <- read.csv2("pequena_peru.csv")
MicroPeru <- read.csv2("micro_peru.csv")
MedianasPeru <- read.csv2("medianas_peru.csv")
GrandesPeru <- read.csv2("grandes_peru.csv")
#COLOMBIA
PequenaColombia <- read.csv2("pequena_colombia.csv")
MicroColombia <- read.csv2("micro_colombia.csv")
MedianasColombia <- read.csv2("medianas_colombia.csv")
GrandesColombia <- read.csv2("grandes_colombia.csv")
#CHILE
PequenaChile <- read.csv2("pequena_chile.csv")
MicroChile <- read.csv2("micro_chile.csv")
MedianasChile <- read.csv2("medianas_chile.csv")
GrandesChile <- read.csv2("grandes_chile.csv")

#creacion de las variables de cada pais
GrandesChile <- mutate(GrandesChile, tamanio = pais)
GrandesColombia <- mutate(GrandesColombia, tamanio = pais)
GrandesPeru <- mutate(GrandesPeru, tamanio = pais)

MedianasChile <- mutate(MedianasChile, tamanio = pais)
MedianasColombia <- mutate(MedianasColombia, tamanio = pais)
MedianasPeru <- mutate(MedianasPeru, tamanio = pais)

MicroChile  <- mutate(MicroChile , tamanio = pais)
MicroColombia <- mutate(MicroColombia, tamanio = pais)
MicroPeru <- mutate(MicroPeru, tamanio = pais)

PequenaChile <- mutate(PequenaChile, tamanio = pais)
PequenaColombia <- mutate(PequenaColombia, tamanio = pais)
PequenaPeru <- mutate(PequenaPeru, tamanio = pais)

#Creacion de tamaño de las empresas
GrandesChile$tamanio[GrandesChile$tamanio == "Chile"] <- "grande"
GrandesColombia$tamanio[GrandesColombia$tamanio == "Colombia"] <- "grande"
GrandesPeru$tamanio[GrandesPeru$tamanio == "Peru"] <- "grande"

MedianasChile$tamanio[MedianasChile$tamanio == "Chile"] <- "mediana"
MedianasColombia$tamanio[MedianasColombia$tamanio == "Colombia"] <- "mediana"
MedianasPeru$tamanio[MedianasPeru$tamanio == "Peru"] <- "mediana"

MicroChile $tamanio[MicroChile $tamanio == "Chile"] <- "micro"
MicroColombia$tamanio[MicroColombia$tamanio == "Colombia"] <- "micro"
MicroPeru$tamanio[MicroPeru$tamanio == "Peru"] <- "micro"

PequenaChile$tamanio[PequenaChile$tamanio == "Chile"] <- "pequena"
PequenaColombia$tamanio[PequenaColombia$tamanio == "Colombia"] <- "pequena"
PequenaPeru$tamanio[PequenaPeru$tamanio == "Peru"] <- "pequena"


#creacion un vector con los atributos de las bases de datos
vectorAtributos <- c("fecha","pais","ingresos",
                     "costos","porcentaje_mujeres",
                     "exportaciones","importaciones",
                     "endeudamiento","morosidad","reservas",
                     "spread","tasa_interes","tamanio")

#empresas grandes
names(GrandesChile) <- vectorAtributos
names(GrandesColombia) <- vectorAtributos
names(GrandesPeru) <- vectorAtributos
#empresas medianas
names(MedianasChile) <- vectorAtributos
names(MedianasColombia) <- vectorAtributos
names(MedianasPeru) <- vectorAtributos
#empresas micro
names(MicroChile) <- vectorAtributos
names(MicroColombia) <- vectorAtributos
names(MicroPeru) <- vectorAtributos
#empresas pequeñas
names(PequenaChile) <- vectorAtributos
names(PequenaColombia) <- vectorAtributos
names(PequenaPeru) <- vectorAtributos

#2) Reuna todas las bases en una sola y defina de qué tipología (tipo de datos) 
#son cada una de las variables que se encuentran en la data.(1 pto) 

empresasAgrup <- GrandesChile%>% 
  union_all(GrandesColombia)%>% 
  union_all(GrandesPeru)%>% 
  union_all(MedianasChile)%>%
  union_all(MedianasColombia)%>% 
  union_all(MedianasPeru)%>% 
  union_all(MicroChile)%>% 
  union_all(MicroColombia)%>%
  union_all(MicroPeru)%>% 
  union_all(PequenaChile)%>% 
  union_all(PequenaColombia)%>% 
  union_all(PequenaPeru)

#union de las bases de datos

str(empresasAgrup)

#3) Determine a través del uso de condicionales y/o for cuántas obervaciones 
#tiene Peru versus Chile.(2 pto)

{ 
  Num_datObs_chile <- 0
  
  for(datos in 1:length(empresasAgrup$pais)){
    if(empresasAgrup $pais[datos] == "chile"){
      Num_datObs_chile <- Num_datObs_chile + 1
    }
    
  } 
  print(paste("Numero de datos observados de Chile:", Num_datObs_chile))
  
  Num_datObs_peru <- 0
  
  for(datos in 1:length(empresasAgrup$pais)){
    if(empresasAgrup$pais[datos]== "peru"){
      Num_datObs_peru <- Num_datObs_peru+1
    }
    
  } 
  print(paste("Numero de datos observados de Peru:", Num_datObs_peru))
}

#4) Determine a través del uso de condicionales y/o for ¿cuál es el país con 
#mayor ingresos de explotación para los años que considera la muestra.(2 pto)

#variables
ingresos.expChile <- 0
ingresos.expPeru <- 0
ingresos.expColombia <- 0
 
  
  
  
  for (datos in 1:length(empresasAgrup$ingresos)){
    if(empresasAgrup$pais[datos] == "chile"){
      ingresos.expChile <- ingresos.expChile + 
        empresasAgrup$ingresos[datos]
      
    }
    if(empresasAgrup$pais[datos] == "peru"){
      ingresos.expPeru <- ingresos.expPeru + 
        empresasAgrup$ingresos[datos]
      
    }
    if(empresasAgrup$pais[datos] == "colombia"){
      ingresos.expColombia <- ingresos.expColombia + 
        empresasAgrup$ingresos[datos]
      
    }
  } 
if(ingresos.expChile > ingresos.expColombia && ingresos.expChile > ingresos.expPeru){
    print(paste("Chile es el pais con mayor ingreso con $",
                ingresos.expChile,"millones de dolares"))
    
  }
if(ingresos.expPeru > ingresos.expChile && ingresos.expPeru > ingresos.expColombia){
    print(paste("Peru es el pais con mayor ingreso con $", 
                ingresos.expPeru, "millones de dolares"))
    
  }
if(ingresos.expColombia > ingresos.expChile && ingresos.expColombia > ingresos.expPeru){
  print(paste("Colombia es el pais con mayor ingreso con $", 
              ingresos.expColombia, "millones de dolares"))
  }
  



#5) Genere una variable(columna) , donde si el país es Chile multiplique la 
#tasa de interes por 0,1, cuando sea Peru le sume 0,3 y, y finalmente si es 
#Colombia divida por 10 (2 ptos).Use condicionales y/o for.


empresasAgrup <- cbind(empresasAgrup, columna = 1)
for(datos in 1:length(empresasAgrup$pais)){
  if(empresasAgrup$pais[datos] == "Chile"){
    empresasAgrup$columna[datos] <- (empresasAgrup$tasa_interes[datos] * 0.1)
    print(empresasAgrup$columna[datos])
  }
  if(empresasAgrup$pais[datos] == "Peru"){
    empresasAgrup$columna[datos] <- (empresasAgrup$tasa_interes[datos] + 0.3)
    print(empresasAgrup$columna[datos])
  }
  if (empresasAgrup$pais[datos] == "Colombia"){
    empresasAgrup$columna[datos] <- (empresasAgrup$tasa_interes[datos]/10)
    print(empresasAgrup$columna[datos])
  }
}

#6) Reemplace en la columna exportaciones con 1 cuando es mayor a 2,1, 
#con un 2 cuando es menor 2,1y un 3 cuando es igual a 2,1, redondee al 
#primer decimal la variable(2 ptos). Use condicionales y/o for.

empresasAgrup$exportaciones <- round(empresasAgrup$exportaciones, 1) 

for (datos in 1:length(empresasAgrup$exportaciones)){
  if(empresasAgrup$exportaciones[datos] > 2.1){
    empresasAgrup$exportaciones[datos] <- 1
  }
  if(empresasAgrup$exportaciones[datos] < 2.1){
    empresasAgrup$exportaciones[datos] <- 2
  }
  if(empresasAgrup$exportaciones[datos] == 2.1){
    empresasAgrup$exportaciones[datos] <- 3
  }
}


#7) Gráfique algunas variables seleccionadas, las cuales puedan responder 
#a una pregunta que se haga con respecto a los datos.


#Graficos: Conocer los distintos niveles de importaciones de cada pais y 
#como es su comportamiento en el periodo 2012-2017

#Tamaño de las importaciones
ggplot(data=empresasAgrup, aes(x=tamanio, y=importaciones, fill=pais)) +
  geom_bar(position = "dodge", stat = "identity") + 
  ggtitle("Niveles de importaciones de empresas de cada pais")
print(paste("Se puede observar que las empresas peruanas poseen el nivel de importaciones mas alto a comparacion de Chile y Colombia"))


#Comportamiento de las impotaciones en el tiempo
empresasAgruptemp <- empresasAgrup
empresasAgruptemp$fecha <- as.Date(empresasAgruptemp$fecha, format="%d-%m-%Y")

ggplot(data = empresasAgruptemp, aes(x=fecha, y=importaciones, group = pais, color= pais)) +
  geom_line() + ggtitle("Comportamiento de las importaciones 2012-2017")
print(paste("En el grafico, se puede observar que las importaciones de las empresas peruanas en comparacion a las de Colombia y las de Chile, poseen un alto y constante nivel importaciones en el periodo entre 2012 y 2017"))

