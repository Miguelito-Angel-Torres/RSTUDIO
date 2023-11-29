# Pruebas parametricas o no parametricas
# Pruebas parametricas: El nivel de medicion de intervalo o razon - Se describa como una curva normal(Campana) -- Muestras Grande + 30  
# recabamos nuestra muestra de una forma aletoria(Toda participante tendra la misma probalidad de ser eligido) entonces referencia Muestra aletoria
# Distribucion Normal (Anova-t student)
# Prueba no parametricas: El nivel de medicion de nominal o ordina - No van a seguir una curva llamada distribucion libre -- Muestra - 30
# Si no utilizamos un muestreo aleotorio entonces Muestra No aletoria, Distribucion Libre (chi-Cuadrada - Wilcoxon)

# Vectores:
c(2,3)*c(4,5)
# class:return que tipo de objeto estoy trabajando
class(c(2,3))
c<-2.1
c<-as.integer(c)
class(c)
e<-c("rojo","azul","azul","amarillo","rojo","anarajando")
e<-as.factor(e)
class(e)
e
#Vectores boleanos
c1<-c(5,1,6)
c1<-c1[c1>=4]
# Matrices
l<-matrix(c(5,6),c(1,2),nrow=2,ncol=2)
?matrix()
class(l)
k<-matrix(c(1,"False",FALSE,0),ncol=2,nrow=2)
## Data Frames
# Estudio 3 sujetos,donde medi tres variable
var1<-c(24,15,16)
var2<-c("H","M","H")
var3<-c(67,78,80)
#Une columnas con numero similar de renglones
cbind() 
#Une renglones con numero similar de columna
rbind()
length(var1)
data1<-cbind(var1,var2,var3)
data2<-rbind(var1,var2,var3)
data3<-data.frame(var1,var2,var3)
data3["var1"]
data3$var1
var1_<-data3$var1
var1_[1]
var5<- data3$var1*data3$var3 
data3$var5<-var5 #con el signo $ puedo insertar variable en un data-frame
data3
#Inspeccionar un data.frame
View(data3)
head(data3,n=2)
str(data3)
# Data cargado en R
data(mtcars)
class(mtcars)
View(mtcars)
head(mtcars,n=2)
str(mtcars) # Estructura de un data frame
summary(data3) # Ver estadisticas descriptiva de un data frame
## Estadisticas descriptivas aplicada R
mean(data3$var1) # Media
sd(data3$var1) # Desviacion estandar
median(data3$var1) # Mediana
quantile(data3$var1) # Cuantiles
quantile(data3$var1,probs=c(0.8,0.85,0.9))
min(data3$var1)
max(data3$var1)
## Categoricas:
table(data3$var1)
prop.table(table(data3$var1))
# Manipulacion de datos con dplyntidyr y el tidyverse
## Instalar paquetes
install.packages("pacman")
install.packages("dplyr") # Ayuda el manejo base de datos 
install.packages("tidyr")
dplyr::filter() ## :: precedido por nombre de paquete me llama una funcion espeofica del paquete
#library(dplyr);library(tidyr)
pacman::p_load(dplyr,tidyr,gapminder,tidyverse,cowplot)
## p_load de el paquete pacman carga e instala rapidamente los paquetes
#pacman::p_loaded()
# Crear datos,guardarlos y cargalos con readr y haven
#library(gapminder) # Es el paquete que contiene el dataset
?gapminder
data("gapminder") ## Carga los datos
dataG<-gapminder ## Asignar gapminder a un objeto
head(dataG)
View(dataG)
# Paquete que sirve para crear datos y cargar base de datos
pacman::p_load(readr,haven,car,ggpubr,viridis,ggsci,ggthemes)
## readr lee,guarda datos en formato .csv .txt
## haven carga datos de varios formato stata,sas,spss,xlxs
setwd("~/Users\Usuario\Desktop\Curso RStudio\AprendiendoRStudio") ## Especificar directorio de trabajo
write_csv(dataG,"datos.csv")
datosGCargado <- read_csv("datos.csv") ## funciones read_csv,read_sas,read_sav para leer datos
View(datosGCargado)
### Manipulando datos con dpylr y tidyr
## Tidyverse
#library(tidyverse) # Tiene varios paquetes:dpylr,tidyr,gglot2
# Pipeline 
#objeto %>% Funcion ## El pipepline toma objeto y los concantena con funciones CTRL + SHIFT + m
str(datosGCargado)
datosGCargado$country<-as.factor(datosGCargado$country) # Recodificando
datosGCargado$country # Recodificacion funciono
datosGCargado$continent<-as.factor(datosGCargado$continent)
datosGCargado$continent
datos_africa<-datosGCargado %>% filter(continent == "Africa")
datos_africa
# write_csv(datos_africa, "datos_africa.csv")
# Me interesa quedarme solo con paises de Africa despues de 1980
datos_africa2<-datosGCargado %>% filter(continent == "Africa" & year > 1980)
datos_africa2
# Me interesa quedarme solo con paises de Africa despues de 1980 o antes de 1960
datos_africa3<-datosGCargado %>% filter((continent == "Africa" & year > 1980) | (continent == "Africa" & year < 1960))
datos_africa3
# Opcion alternativo: datos_africax <- datosGCargado %>% filter(continent == "Africa") %>% filter(year 1980 | year < 1960)
## Promedio de expectativa de vida por continente por año
# apply es una funcion que va permitir aplicar funcion sobre todo un conjunto de datos
tapply(datosGCargado$lifeExp, datosGCargado$continent,mean)
tapply(datosGCargado$lifeExp,datosGCargado$continent,sd)
tapply(datosGCargado$lifeExp,datosGCargado$continent,min)
tapply(datosGCargado$lifeExp,datosGCargado$continent,max)
# group,sumarise

datos_agrupados1<- datosGCargado %>% group_by(continent) %>% summarise(meanExp=mean(lifeExp))
datos_agrupados2<- datosGCargado %>% group_by(continent,year) %>% summarise(meanExp=mean(lifeExp))
class(datos_agrupados1) # Objetos en R puede tener mas de una clase
datos_agrupados2
datosGCargado %>% group_by(continent) %>% summarise(sumanotoria=sum(lifeExp))
## Dos agrupaciones, cinco estadisticas de resumen
datos_agrupados3<- datosGCargado %>% group_by(year,continent) %>% summarise(meanExp=mean(lifeExp),
                                                                            sdExp=sd(lifeExp),minExp=min(lifeExp),
                                                                            maxExp=max(lifeExp),medianExp=median(lifeExp))
datos_agrupados3
write_csv(datos_agrupados3,"resumen.csv")
datosCargado1<-read.csv("resumen.csv")
View(datosCargado1)
# Dos agrupaciones,cinco estadisticas de resumen,ordenado por el minimo y maximo
## arrange SOLO ORDENA DE MENOR A MAYOR
## arrange(desc(x)) ordena x de mayor a menor
datos_agrupados4<- datosGCargado %>% group_by(year,continent) %>% summarise(meanExp=mean(lifeExp),
                                                                            sdExp=sd(lifeExp),minExp=min(lifeExp),
                                                                            maxExp=max(lifeExp),medianExp=median(lifeExp)) %>% arrange(desc(minExp)) %>%
                                                                          mutate(ciInf=meanExp-1.96*sdExp,ciSup=meanExp+1.96*sdExp)

?arrange
datos_agrupados4

## Reproducibilidad de procesos aletorios
set.seed(123);
datos_muestra <- datosGCargado %>% sample_n(10,replace=FALSE)
datos_muestra
## SELECT
names(datosGCargado) ##NOMBRE DE LAS VARIABLES
datosGCargado
datosGCargado %>% select(country,continent,lifeExp,pop) %>% filter(lifeExp>30)
datosGCargado %>% filter(year>1970) %>% select(country,lifeExp,pop)
## Mutate ## CREAR NUEVAS VARIABLES
DATOS_GDP <- datosGCargado %>% mutate(gdp=gdpPercap * pop)
DATOS_GDP
datosGCargado$gbp1 <- datosGCargado$gdpPercap * datosGCargado$pop
datosGCargado

# left_join hace referencia a la tabla izquiera donde va unir con la tabla derecha referencia a una columna espec
# asi dando toda la informacion de la tabla derecha
a1<-c("A","B","C")
b1<-c(1,2,3)
df1<-data.frame(a1,b1)
a1<-c("A","B","D")
b2<-c(43,56,78)
df2<-data.frame(a1,b2)
df1
df2

df3<-df1 %>% left_join(df2,by="a1")
df3
df4 <-df1 %>% right_join(df2,by="a1")
df4
df5 <- df1 %>%inner_join(df2,by="a1") %>% select(b1)
df5
df6 <-df1 %>%full_join(df2,by="a1") 
df6
## Manipulando fechas (DOCUMEBTACION DEL PAQUETE LUBRIDATE)
#################################################
##################### Realizacion de Practica #############################################
data(iris) # Cargar base de datos
iris # base de datos completa
head(iris) #Ver los primeras 6 filas de la base
tail(iris) # Ver los ultimoas 6 filas de la base
iris$Species
table(iris$Species)
iris$Sepal.Length
summary(iris$Sepal.Length)
iris$Sepal.Width
summary(iris$Sepal.Width)
iris$Petal.Length
summary(iris$Petal.Length)
iris$Petal.Width
summary(iris$Petal.Width)
###### Graficas predeterminada en R ###############
hist(iris$Sepal.Length) #Histograma
plot(iris$Sepal.Length,iris$Sepal.Width) # Grafico de dispersion
boxplot(iris$Sepal.Length~iris$Species) # Grafico de Cajones
hist(iris$Sepal.Length, # Variable a graficar
    col="midnightblue", #Color
    border="white", #Borde
    main="Distribucion de la longitud de Sepalo", #Titulo
    xlab="Longitud del sepalo",ylab="Conteo(Frecunecia)") # Etiquetas de los ejes
plot(iris$Sepal.Length,iris$Sepal.Width,
     col="gold2",
     pch=16, #Forma del punto
     cex=1.15, # Tamaño del punto
     main="Relacion largo-ancho del petalo",
     xlab="Longitud",ylab="Anchura",
     ); abline(lm(Petal.Width~Petal.Length,iris),col="gold4") # Linea de regresion
boxplot(iris$Sepal.Length~iris$Species,
        col=c("indianred1","red2","red4"), # Colores (vector para especificar varios)
        main="Longitud de sepalo segun la especie", #TiTulo
        xlab="Especie",ylab="Longitud") #Etiquetas

# Descargar el paquete tidyverse para grafico ggplot2
### Para funcionar ggplot requiere 3 cosas:
# 1 - Base de datos 
# 2-Variables que se van a graficar: mapping=aes()
ggplot(data=iris,mapping= aes(x=Sepal.Length))
# 3 - Parametro grafico al que se mapean los datos (funciones "geom"): CAPAS
ggplot(data=iris,mapping=aes(x=Sepal.Length)) + geom_histogram()
# Elegir el tipo de grafico de acuerdo con los tipos de variables
# Una variable continua
ggplot(data=iris,mapping=aes(x=Sepal.Length)) + geom_histogram()
ggplot(data=iris,mapping=aes(x=Sepal.Length)) + geom_density()
ggplot(data=iris,mapping=aes(x=Sepal.Length)) + geom_dotplot()
ggplot(data=iris,mapping=aes(x=Sepal.Length)) + geom_freqpoly()
# Una variable de categorica
ggplot(data=iris,mapping=aes(x=Species)) + geom_bar()

data1 <- data.frame("Respuesta"=c(LETTERS[1:4]),"N_alumnos"=c(10,2,5,11))
data1
ggplot(data=data1,mapping=aes(x=Respuesta)) + geom_bar()
# stat = argumento que indica que transformacion estadistica se le hara a los datos
# stat=bin: separa una variable continua en bloques y cuenta la frecuencia de esos bloques
# stat=count: cuenta el numero de veces que se repite una varible
# stat=identify:deja las variables tal cual como estan
ggplot(data=data1,mapping=aes(x=Respuesta)) + geom_bar() #stat=count
# Solucion 1: usar un geom que tenga por default el stat que necesitamos
ggplot(data=data1,mapping=aes(y=N_alumnos,x=Respuesta)) + geom_col()
ggplot(data=iris,mapping=aes(y=Sepal.Length,x=Species)) + geom_col()
# Solucion 2 : Usar la misma funcion de antes pero especificar el stat
ggplot(data=iris,mapping=aes(y=Sepal.Length,x=Species)) + geom_bar(stat="identify") # No realiza
# Solucion 3 : Usar una funcion "stat" en vez de una funcion geom
ggplot(data=iris,mapping=aes(y=Sepal.Length,x=Species)) + stat_identity(geom="bar")
ggplot(data=data1,mapping=aes(y=N_alumnos,x=Respuesta)) + stat_identity(geom="bar")
# Cambiar el stat predeterminado aplica para muchas funciones (geom_bar, geom_area)
####Dos variables continuas####
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width)) + geom_point()
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width)) + geom_jitter()

ggplot(data=iris,mapping = aes(x=Sepal.Length,y=Sepal.Width)) + geom_point()
ggplot(data=iris,mapping = aes(x=Sepal.Length,y=Sepal.Width)) + geom_jitter()

ggplot(data=iris,mapping = aes(x=Sepal.Length,y=Sepal.Width)) + geom_quantile()

ggplot(data=iris,mapping = aes(x=Sepal.Length,y=Sepal.Width)) + geom_quantile(quantiles=c(0.2,0.3,0.4,0.5))

ggplot(data=iris,mapping= aes(x=Sepal.Length,y=Sepal.Width)) + geom_smooth()
ggplot(data=iris,mapping= aes(x=Sepal.Length,y=Sepal.Width)) + geom_smooth() + geom_quantile(method = "lm")
#y = x al cuadrado
ggplot(data=iris,mapping= aes(x=Sepal.Length,y=Sepal.Width)) + geom_smooth(formula = y~poly(x,2))
set.seed(123);data2 <- (data.frame("X"=runif(n=1000,min=1,max=100))) %>% mutate("Y"=(X**2))
ggplot(data=data2,mapping=aes(x=X,y=Y)) + geom_smooth(formula= y~poly(x,2))
ggplot(data=iris,mapping=aes(x=Sepal.Length,y=Sepal.Width)) + geom_rug() + geom_point()

######Una variable continua y una categorica ####
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_point()
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_jitter()
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_col()
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_boxplot()
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_violin()
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_dotplot(binaxis="y",stackdir="center")

####COMBINAR CAPAS####
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width)) +  geom_rug() + geom_point() +
  geom_smooth(method = "lm")

anot1<-as.vector(tapply(iris$Petal.Width,iris$Species,median))
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_boxplot() + geom_point() +
  annotate(geom="text",x=c(1,2,3),y=c(1,2,3),label=anot1)

ggplot(data=data1,mapping=aes(y=N_alumnos,x=Respuesta)) + geom_col() + geom_text(label=c("A","B","C","D")) #Porcentaje de alumnos que constestaron ese incisco

####FACETING####
data(mpg)
?mpg
table(mpg$manufacturer)
table(mpg$year)
table(mpg$cyl)
table(mpg$fl)
table(mpg$class)
summary(mpg$displ)
ggplot(mpg,aes(displ,hwy)) + geom_point()
ggplot(mpg,aes(displ,hwy)) + geom_point() + facet_grid(rows=vars(year))
ggplot(mpg,aes(displ,hwy)) + geom_point() + facet_grid(cols=vars(year))
ggplot(mpg,aes(displ,hwy)) + geom_point() + facet_grid(~year,nrow=1,ncol=2)
ggplot(mpg,aes(displ,hwy)) + geom_point() + facet_grid(rows=vars(cyl),scales="free")
ggplot(mpg,aes(displ,hwy)) + geom_point() + facet_grid(cols=vars(cyl))

ggplot(mpg,aes(displ,hwy)) + geom_point() + facet_grid(cyl~year)
ggplot(mpg,aes(displ,hwy)) + geom_point() + facet_wrap(cyl~year,nrow=4)

#### Grafico con ggplto2:agrupar y guardar ####
Fig1A <- ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_boxplot()
Fig1B <- ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width)) + geom_jitter() +
  geom_smooth(method = "lm")
ggpubr::ggarrange(Fig1A,Fig1B,ncol=2,nrow=1,labels = c("A","B"))

F1 <- ggarrange(Fig1A,Fig1B)
ggsave(filename="F1.png",plot=F1,width=35,height = 20,units=c("cm"),dpi=200,limitsize = FALSE)
#### Graficos con ggplot2:personalidzacion ####
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width)) + geom_boxplot()
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width)) + geom_point()
# Color,fill,alpha,shape,linetype
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width,fill=Species,color=Species)) + geom_point() +
  geom_boxplot(color="black")   
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width,color=Species,linetype=Species)) + geom_point(alpha=6,shape=18,size=3) +
  geom_smooth(method = "lm",color="midnightblue")
# Scale manual (values,name,limits,breaks,labels)
ggplot(data=iris,mapping = aes(x=Species,y=Petal.Width,fill=Species,color=Species)) +
  geom_point() + geom_boxplot(color="black") + scale_fill_manual(values=c("indianred1","red2","red4"),name="Flor",labels=c("setosa","Versicolor","Virginica")) +
  scale_color_manual(values=c("indianred1","red2","red4"),name="Flor",labels=c("setosa","Versicolor","Virginica"))
# Scale (brewer,grey,ggsci) <- categoricas
# Scale gradient,viridis <- continuas
ggplot(data=iris,mapping=aes(x=Species,y=Petal.Width,fill=Species)) +
  geom_boxplot() + scale_fill_grey() + scale_color_grey()
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Petal.Width))+
  geom_point(alpha=0.6,shape=16) + 
  geom_smooth(method = "lm",color="red") + scale_color_viridis()

ggplot(data=iris,mapping = aes(x=Species,y=Petal.Width,fill=Species))+
  geom_boxplot(color="black",size=0.75) +
  scale_fill_manual(values=c("indianred1","red2","red4"),labels=c("F1","F2","F3"),name="Tipo \n de flor")

ggplot(data=iris,mapping = aes(x=Species,y=Petal.Width,fill=Species))+
  geom_boxplot(color="black",size=0.75) +
  scale_fill_manual(values=c("indianred1","red2","red4"),name="Tipo \n de flor",
                    limits=c("virginica","setosa","versicolor"))
# Se pueden cambiar las variables con paletas predeterminadas 
## PALETAS PARA VARIABLES CATEGORICAS
## PAQUETE GGPLOT2
# Scale brewer
ggplot(data=mpg,mapping = aes(x=class,y=hwy,fill=class)) +
  geom_point() + geom_boxplot(color="black") + ggplot2::scale_fill_brewer(palette="Spectral",name="Tipo",
                                                                          labels=LETTERS[1:7])

RColorBrewer::display.brewer.all()
# Scale discrete
ggplot(data=mpg,mapping = aes(x=class,y=hwy,fill=class)) +
  geom_point() + geom_boxplot(color="black") + ggplot2::scale_fill_discrete()
#PAQUETE GGTHEMES
# scale calc
ggplot(data=mpg,mapping = aes(x=class,y=hwy,fill=class)) +
  geom_point() + geom_boxplot(color="black") + ggthemes::scale_fill_calc()
## PALETAS PARA VARIABLES CONTINUAS;
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Petal.Width)) + geom_point(size=2) +
  ggplot2::scale_color_viridis_c(option = "A")
# Logaritmo
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Petal.Width)) + geom_point(size=2) +
  ggplot2::scale_color_viridis_c(option = "A") + scale_x_log10()
# Raiz cuadrada
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Petal.Width)) + geom_point(size=2) +
  ggplot2::scale_color_viridis_c(option = "A") + scale_y_sqrt()
#Inverso
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Petal.Width)) + geom_point(size=2) +
  ggplot2::scale_color_viridis_c(option = "A") + scale_x_reverse()
# Separar en segmentos
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Petal.Width)) + geom_point(size=2) +
  ggplot2::scale_color_viridis_c(option = "A") + scale_y_binned()
# Convertir en tiempo
ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Petal.Width)) + geom_point(size=2) +
  ggplot2::scale_color_viridis_c(option = "A") + scale_x_time() + scale_y_time()
####- Graficos con ggplot2:titulos,etiquetas, temas
ggplot(data=iris,mapping=aes(x=Sepal.Length,fill=Species)) +
  geom_density(color="black",size=1,linetype=5,alpha=0.75) +
  scale_fill_viridis(option="C",discrete=TRUE,name="Tipo \n de Flor") +
  ggtitle("Distribución de la longitud de sepalo") + 
  xlab("Longitud de Sepalo") + ylab("Densidad")
# TEMAS
# paquetes ggplot
ggplot(data=iris,mapping = aes(x=Sepal.Length,fill=Species)) + geom_density(alpa=0.75) +
  scale_fill_viridis(option = "C",discrete = TRUE) + ggplot2::theme_get()
# paquete ggppubr
ggplot(data=iris,mapping = aes(x=Sepal.Length,fill=Species)) + geom_density(alpa=0.75) +
  scale_fill_viridis(option = "C",discrete = TRUE) + ggpubr::theme_cleveland()
# paquete ggthemes
ggplot(data=iris,mapping = aes(x=Sepal.Length,fill=Species)) + geom_density(alpa=0.75) +
  scale_fill_viridis(option = "C",discrete = TRUE) + ggthemes::theme_stata()
ggplot(data=iris,mapping = aes(x=Sepal.Length,fill=Species)) + geom_density(alpa=0.75) +
  scale_fill_viridis(option = "C",discrete = TRUE) + ggthemes::theme_economist()
# Todo junto:
F1A <- ggplot(data=iris,mapping = aes(x=Sepal.Length,fill=Species)) + geom_density(color="black",size=1,linetype=5,alpha=0.75) +
  scale_fill_viridis(option = "C",discrete = TRUE,name="Tipo \n de flor") +
  ggtitle("Distribución de la \nlongitud de sepalo") + xlab("Longitud de sepalo") + ylab("Densidad") +
  ggpubr::theme_pubclean()
F1B <- ggplot(data=iris,mapping = aes(x=Species,y=Sepal.Width,fill=Species)) + geom_point(size=1.9,shape=21) + geom_boxplot(color="black",size=0.75) +
  scale_fill_viridis(option = "C",discrete = TRUE,name="Tipo \n de flor") +
  scale_color_viridis(option = "C",discrete = TRUE,name="Tipo \n de flor") +
  ggtitle("Anchura de sepalo para \n cada tipo de flor") + xlab("") + ylab("Anchura de SePalo") +
  ggpubr::theme_pubclean()
F1C <- ggplot(data=iris,mapping = aes(x=Petal.Length,y=Petal.Width,color=Sepal.Width)) + 
  geom_point(size=2,alpha=0.75) + scale_colour_viridis_b(option="C",name="Anchura\nde sepalo") + 
  geom_smooth(color="black",method = "lm") + geom_rug(color="black") + ggpubr::theme_pubclean() +
  ggtitle("Relacion longitud-anchura \nde petalo") + xlab("Longitud de petalo") + ylab("Anchura de petalo")
# Funcion "theme para modificar el formato de la grafica:
# Borrar elementos : elemento_blank
F1A + theme(plot.title = element_blank()) # Titulo
F1A + theme(axis.text.x = element_blank(),axis.text.y = element_blank() ) # Texto de Eje
F1A + theme(axis.title.x = element_blank(),axis.title.y = element_blank() ) # Titulo de Eje
F1A + theme(legend.text = element_blank()) # Texto de la leyenda
F1A + theme(legend.title = element_blank()) # Titulo de la leyenda
# Borrar todo
F1A + theme(plot.title = element_blank(),axis.title = element_blank(),
            axis.text = element_blank(),axis.ticks = element_blank(),
            legend.position = "none")
#Cambiar posicion de la leyenda
F1A + theme(legend.position = "left")
#Element_text:fuente,formato,color,tamaño,just horizontal,just vertical,angulo
F1A + theme(plot.title = element_text(family = "serif"))
F1A + theme(plot.title = element_text(face = "plain"))
F1A + theme(plot.title = element_text(colour = "red4"))
F1A + theme(plot.title = element_text(size=10))
F1A + theme(plot.title = element_text(size=11,hjust=0)) # Izquierda
F1A + theme(plot.title = element_text(size=11,hjust=1)) # Derecha
F1A + theme(plot.title = element_text(size=11,hjust=0.5)) # Centrado
F1A + theme(plot.title = element_text(size=11,hjust=0.5,vjust=0))
F1A + theme(plot.title = element_text(size=11,hjust=0.5,vjust=1))
F1A + theme(plot.title = element_text(angle = 360,hjust = 0.5))
### Grafico con ggplot2:anotaciones,comparaciones
# Paste0() pega los objetos que se le indiquen como caracteres sin dejar espacios
paste0("Cruz ","Azul ","campeón ",2021)
round(3.145555555,4)
round((1:10)/7,2)
paste0(1:10," entre 7 es igual a", round((1:10)/7,2))
# HACER ANOTACIONES PARA CADA GRAFICO
F1A <- F1A + theme(plot.title = element_text(face="bold",hjust=0.5,size=15)); F1A
# Mediana de longitud de sepalo para cada especie
ann1 <- tapply(iris$Sepal.Length,iris$Species,median)
ann1 <- paste0("M=",ann1)
F1A <-F1A +  annotate("text",x=c(5,5.8,6.5),y=c(1.3,0.85,0.8),label=ann1,size=3.5)
# Coeficiente de correlacion
ann2 <- cor(x=iris$Petal.Length,y=iris$Petal.Width,method = "spearman")
ann2 <- round(ann2,3)
F1C + annotate("text",x=2.25,y=2,label=ann2,size=3.5)
# stat compare means permite hacer pruebas de hipotesis y plasmar el resultado en la grafica
F1B + ggpubr::stat_compare_means(label.x=2,label.y = 4.25,size=3.5,method = "anova")
F1B + ggpubr::stat_compare_means(size=3.5,comparisons = list(c("setosa","virginica"),
                                                             c("setosa","versicolor"),
                                                             c("versicolor","virginica")),
                                 symnum.args = list(cutpoints=c(0,0.001,0.01,0.05,0.1),
                                                    symbols=c("***","**","*",".")))
F1C <- F1C + annotate("text",x=2.25,y=2,label=ann2,size=3.5)
### -- Grafico con ggplot2:agrupar y guardar
F1 <- ggarrange(F1A,F1B,F1C,ncol=3,nrow=1,labels=LETTERS[1:3])
F1
ggsave(filename="F1x.png",plot=F1,
        units=c("cm"),width=45,height=20,
        dpi=200,limitsize=FALSE)

setwd("C:/Users/Usuario/Desktop/Curso RStudio/AprendiendoRStudio")
# Juntar con dplyr
ggplot(mpg,aes(displ,hwy)) + geom_point() +  facet_grid(rows = vars(year))
                                                        mpg %>% ggplot(aes(displ,hwy)) + geom_point() + facet_grid(rows = vars(year))
iris2 <- iris %>% filter(Sepal.Length>=5.5) %>% ggplot(data=iris2,mapping = aes(x=Species)) + geom_bar()
set.seed(123);data2 <- (data.frame("X"=runif(n=1000,min=1,max=100))) %>% mutate("Y"=(X**2)) 
ggplot(data=data2,mapping = aes(x=X,y=Y)) + geom_smooth(formula = y~poly(x,2))

mpg %>% filter(cyl!=5) %>% mutate("year2"=factor(year)) %>% ggplot(aes(x=cyl,y=hwy,fill=year2)) + geom_col(position = "dodge") # stack.fill.dodge
### PRUEBAS DE HIPOTESIS Y ANOVA DE UNA VIA EN R (P-VALUES)
### Un valor de p representa la probalidad bajo un modelo estadistico de que un estadistico obtenido de una
### poblacion seria igual o mas extremo que el valor observado
### Un valor de p indica que tan incompatible son los datos con un modelo estadistico especifico
### El modelo generalmente se acompaña de una hiptesis nula
### Entre mas pequeño el valor del estadistico p,mayor la incompatibilidad de los datos con el modelo
### Los valores de p no miden la probabilidad de que la hipotesis alternativa es verdadera o que los 
### obtenido se deban al azar,El valor de p es una conclusion estadistica de los datos,NO DE LA HIPOTESIS
### p<0.05 NO IMPLICA QUE UN EFECTO SEA REAL
### EL TAMAÑO DE P DEPENDE DEL TAMAÑO DE LA MUESTRA Y DE LA PRECISION DE LA MEDICION
pacman::p_load(tidyverse,ggpubr,haven,nortest,dunn.test,datasets,vcd,moments,car,PairedData)
### Chi cuadrada
#A:Descripcion de la prueba
# Utilidad
# La prueba de independencia de chi-cuadrado se utiliza para analizar tablas de frecuencias(es decir,la tabla de contigencia) formado por dos
#La prueba de chi-cuadrado evalua si existe una asociacion significativa entre las categorias de dos variables
# Resumen de Hipotesis:
#Hipotesis nula(H0): las variables de fila y columna de la tabla de contingencia son independientes
#Hipotesis alternativa(h1):las variables de fila y columna son dependientes
# Supuesto de la prueba
# Cada observacion es independiente de todos los demas(es decir,una observacion por sujeto);
# No mas de 20% de los recuentos esperados deben ser menores de 5,en cuyo caso se sugiere usar una correcion de Yates.
# Todos los recuentos esperados individuales deben ser cuando menos de 1 o más
# 1 Dataset o trabajar
df_chi <- MASS::survey
?MASS::survey
head(df_chi)
nrow(df_chi)
df_chi_1 <- df_chi %>% dplyr::select(Sex,Exer,Smoke)
nrow(df_chi_1)
ncol(df_chi_1)
head(df_chi_1)
#Evaluar frecuencias en tablas
summary(df_chi_1)
table(df_chi_1$Smoke) # Evaluar Conteo
sort(table(df_chi_1$Smoke,useNA="always"),decreasing = TRUE)
table(df_chi_1$Smoke,exclude = "Never") # Excluir algun elemento del conteo
prop.table(table(df_chi_1$Smoke,useNA="always")) # Evaluar Proporcion
round(prop.table(table(df_chi_1$Smoke)) * 100,1) # Porcentaje
round(prop.table(table(df_chi_1$Smoke,useNA="always")) * 100,1)
round(prop.table(table(df_chi_1$Smoke,exclude ="Never")) * 100,1)
## Evaluar tablas de contigencia
table(df_chi_1$Smoke,df_chi_1$Exer) # Incluir dos variables
table(df_chi_1$Smoke,df_chi_1$Exer,df_chi_1$Sex) # Incluir una tercera variable como estratificacion
table(df_chi_1$Smoke,df_chi_1$Exer,df_chi_1$Sex,useNA = "always")
table(df_chi_1$Smoke,df_chi_1$Exer,useNA = "always")
# Recom:Tu primer objeto que sea tu variable a explotar,tu segundo objeto que sea tus grupos a explorar
# Evaluar proporciones de acuerdo al grupo de estudio
round(prop.table(table(df_chi_1$Smoke,df_chi_1$Exer)) * 100,1) #Evaluar proporcion de acuerdo a la unidad
round(prop.table(table(df_chi_1$Smoke,df_chi_1$Exer),1) * 100,1) # acuerdo fila
round(prop.table(table(df_chi_1$Smoke,df_chi_1$Exer),2) * 100,1) # acuerdo columna
round(prop.table(table(df_chi_1$Smoke,df_chi_1$Exer,df_chi_1$Sex),2) * 100,1)
# Hacer graficas de proporciones
ggplot(data=df_chi_1,aes(x=Exer,y=..prop..,group=1)) + geom_bar(stat="count") +
  scale_y_continuous(labels=scales::percent_format())
ggplot(data=df_chi_1,aes(x=Smoke,y=..prop..,group=1)) + geom_bar(stat="count") +
  scale_y_continuous(labels=scales::percent_format())
# HACER GRAFICOS DE BARRA DE ACUERDO A LA FRECUENCIA
ggplot(df_chi_1,aes(x=Exer,fill=Smoke))+ geom_bar(position = "dodge")
ggplot(df_chi_1,aes(x=Exer,fill=Smoke)) + geom_bar(position = "fill") + labs(y="proportion")
df_chi_1 %>% na.omit() %>% ggplot(aes(x=Exer,fill=Smoke)) + geom_bar(position = "fill") +
  labs(y="proportion") + facet_wrap(~Sex)
#3 Definir la prueba a usar de acuerdo a pregunta
# El ejercicio de los estudiantes es independiente del sexo
tab1 <- table(df_chi_1$Exer,df_chi_1$Sex)
chisq.test(tab1) # x-squared
tab1 <- table((df_chi_1$Exer=="Freq"),df_chi_1$Sex)
chisq.test(tab1)
tab1 <- table((df_chi_1$Exer=="Freq" | df_chi_1$Exer=="Some"),df_chi_1$Sex)
chisq.test(tab1)
# El fumar en los estudiantes es independiente del Sexo
tab1 <- table(df_chi_1$Smoke,df_chi_1$Sex)
chisq.test(tab1)
tab1 <- table((df_chi$Smoke!="Never"),df_chi_1$Sex)
chisq.test(tab1)
tab1 <- table((df_chi_1$Smoke=="Regul" | df_chi_1$Smoke== "Heavy" | df_chi_1$Smoke == "Never"),df_chi_1$Sex)
tab1
chisq.test(tab1,correct = T)
# Descripcion de la prueba normalidad
#Utilidad
# La normalidad y otros suposiciones hechas por estas pruebas deben tomarse para sacar una interpretacion y conclusiones
# confiable en el analis descripcion.
# Antes de utilizar una prueba parametrica,debemos realizar algunas pruebas preiliminares para asegurarnos de que se
# cumplen los supuestos de la prueba.
# En las situaciones en las que se violan los supuestos,se recomiendan pruebas no parametricas
#---Resumen de la Hipotesis:
#Hipotesis nula(H0):Las variables a analizar siguen un patron normal establecido
# Hipotesis alternativa(HI):Las variables a analizar siguen una distribucion ajena a un histograma normal.
df_norm <- MASS::survey
?MASS::survey
head(df_norm)
df_norm_1 <- df_norm %>% dplyr::select(Pulse,Height,Age)
summary(df_norm_1)
# Como se ve una grafica con distribucion simetrica,normal o "parametrica"?
x <- seq(-4,4,length=100)
hx <- dnorm(x)
hx
# plot a standard normal distribution
plot(x,hx,type="l",lty=2,xlab="x value")
# plot a vertical line at-2*std
abline(v=-2,col="red")
# plot a vertical line at 2*std
abline(v=2,col="red")
# make the arrow
arrows(x0=-2,y0=0.35,x1=2,y1=0.35,code=3,col="blue")
# Histogramas:Si sus datos se ven como una curva de campana,porbablemente sea normal
ggplot(df_norm_1,aes(x=Pulse)) + geom_histogram()
hist(df_norm_1$Pulse)
ggplot(df_norm_1,aes(x=Age)) + geom_histogram()
hist(df_norm_1$Age)
# Grafica de densidad: Grafico similares a un histograma,que sombrea el area debajo de los datos
ggdensity(df_norm_1$Pulse,fill="red")
ggdensity(df_norm_1$Age,fill="red")
# Grafica Quantil-Quantil(QQPLOT): Grafica que "estira" los datos con respecto a una curva de normalidad
ggqqplot(df_norm_1$Pulse)
ggqqplot(df_norm_1$Age)
#Evaluar el sesgo y la kurtasis de un histograma
#La distribucion normal tiene un sesgo de 0;Positivas el sesgo es a la "derecha",negativo el sesgo es a la izquierda
# Los distribuciones con curtosis <=3 son platicurticos y >3 son leptocurticos.

# Evaluar el histograma de Puslo,curtosis el sesgo
skewness(df_norm_1$Pulse,na.rm=TRUE)
kurtosis(df_norm_1$Pulse,na.rm = TRUE)

skewness(df_norm_1$Age,na.rm=TRUE)
kurtosis(df_norm_1$Age,na.rm=TRUE)
# Prueba de Agostino para evaluar sesgo
agostino.test(df_norm_1$Age)
# Definir prueba a usar Recordar presupuesto
# Prueba basada en Correlacion
#Shapiro Wilk(sw-test;shapiro.test)
#Originalmente validado para tamaño muestrales de 3>n<500
#Los valores pequeños de SW conducen al rechazo de una distribucion normalidad,mientras que un valor que tiende a 1
shapiro.test(df_norm_1$Pulse)
shapiro.test(df_norm_1$Age)
#Kolmogorov-Smirnov(KS test;ks.test)
ks.test(df_norm_1$Pulse,"pnorm")
# Mejor aplicar este metodo que Kolmogorov
#Anderson-Prueba de Darling (AD-test)
nortest::ad.test(df_norm_1$Pulse)
nortest::ad.test(df_norm_1$Age)
# T muestras independientes
df_t<-MASS::survey
head(df_t)
df_t_1 <- df_t %>% dplyr::select(Sex,Height,Age,Pulse)
df_t_1
summary(df_t_1)
# Histograma
ggplot(df_t_1,aes(x=Pulse)) + geom_histogram() + facet_wrap(~Sex)
ggplot(df_t_1,aes(x=Age)) + geom_histogram() + facet_wrap(~Sex)
# Grafico de qqPLOT
ggqqplot(df_t_1,x="Pulse",facet.by = "Sex")
ggqqplot(df_t_1,x="Height",facet.by = "Sex")
ggqqplot(df_t_1,x="Age",facet.by = "Sex")
# Prueba de SW
shapiro.test(df_t_1[df_t_1$Sex=="Male",]$Pulse) # Distribucion de ambas variables sigue una distribucion simetrica
shapiro.test(df_t_1[df_t_1$Sex=="Female",]$Pulse)

shapiro.test(df_t_1[df_t_1$Sex=="Male",]$Age)
shapiro.test(df_t_1[df_t_1$Sex=="Female",]$Age) # Distribucion de ambas variables completamente asimetrica

#Prueba de AD
nortest::ad.test(df_t_1[df_t_1$Sex=="Male",]$Pulse)
nortest::ad.test(df_t_1[df_t_1$Sex=="Female",]$Pulse)

nortest::ad.test(df_t_1[df_t_1$Sex=="Male",]$Age)
nortest::ad.test(df_t_1[df_t_1$Sex=="Female",]$Age)

# 3 Definir prueba a usar. Recordar supuestos.
## Pregunta: ¿ Existiran diferencia de pulso,estatura,edad entre sexo?

# Evaluar homogeneidad de la varianza

# Hipotesis nula(Ho): La varianza entre grupo es similar
# Hipotesis alternativa(H1): hay una diferencia entre las varianzas de los dos grupos
# NOTA DE IMPORTANTE: la prueba de t,asume igualdad de varianzas
#PRUEBA F(var.test)
#El estadistico de la prueba F se puede obtener calculando la razon de las dos varianzas VAR(A)/VAR(B)
#Cuanto mas se desvia esta relacion de 1,mas fuerte es la evidencia de variaciones desiguales de la poblacion
var.test(Pulse~Sex,data=df_t_1)
var.test(Height~Sex,data=df_t_1)
var.test(Age~Sex,data=df_t_1)
#Prueba de Levene(LeveneTest)
#La prueba de Levene es una alternativa a la prueba de Bartlett cuando los datos no se distribuyen simetricamente
leveneTest(Pulse~Sex,data=df_t_1)
leveneTest(Height~Sex,data=df_t_1)
leveneTest(Age~Sex,data=df_t_1)
# Evaluar prueba t "clasico"
# Por default, la prueba asume una correcion de Welch
t.test(Pulse~Sex,data=df_t_1)
t.test(Height~Sex,data=df_t_1)
t.test(Age~Sex,data=df_t_1)
# La correcion puede modificarse
t.test(Pulse~Sex,data=df_t_1,var.equal=TRUE)
t.test(Height~Sex,data=df_t_1,var.equal=TRUE)
t.test(Age~Sex,data=df_t_1,var.equal=TRUE)
# Realizar un grafico de ggplot
df_t_1 %>% filter(!is.na(Sex)) %>% ggplot(aes(x=Sex,y=Pulse)) + geom_boxplot() + 
  stat_compare_means(method = "t.test")
df_t_1 %>% filter(!is.na(Sex)) %>% ggplot(aes(x=Sex,y=Height)) + geom_boxplot() + 
  stat_compare_means(method = "t.test")
df_t_1 %>% filter(!is.na(Sex)) %>% ggplot(aes(x=Sex,y=Age)) + geom_boxplot() + 
  stat_compare_means(method = "t.test")
## T pareado
# Se utiliza para comparar las medias entre dos grupos relacionados
# En este caso,se tiene dos valores medidos para la misma muestras.
# Resumen de Hipotesis
# B. Supuestos de la prueba
# Datos Continuos:Los datos son continuos(No discretos)
# Simetria de los datos:Los datos,es decir,las diferencias para los pares emparejados.
# Muestra Representativo: Cada individuo de la poblacion tiene la misma probabilidad de ser seleccionado en la muestra
#1 Dataset a trabajar
# Weight of the mice before treatment
before <- c(200.1,190.9,192.7,213,241.4,196.9,172.2,185.5,205.2,193.7)
# Weight of the mice after treatment
after <- c(392.9,393.2,345.1,393,434,427.9,422,383.9,392.3,352.2)
# Create a data frame
df_tp_1 <- data.frame(group = rep(c("before","after"),each=10),weight = c(before,after))
# Visualizacion de la informacion
summary(df_tp_1)
# Histogramas
ggplot(df_tp_1,aes(x=weight)) + geom_histogram() + facet_wrap(~group)
# Grafico ggplot
ggqqplot(df_tp_1,x="weight",facet.by = "group")
#Prueba de normalidad
shapiro.test(df_tp_1[df_tp_1$group=="after",]$weight)
shapiro.test(df_tp_1[df_tp_1$group=="before",]$weight)
nortest::ad.test(df_tp_1[df_tp_1$group=="after",]$weight)
nortest::ad.test(df_tp_1[df_tp_1$group=="before",]$weight)
# Evaluar varianza de los datos
var.test(weight~group,data=df_tp_1)
leveneTest(weight~group,data=df_tp_1)
# Aplicar Prueba de T pareado
t.test(weight~group,data= df_tp_1)
# Realizar una grafico de ggplot
ggpaired(df_tp_1,x="group",y="weight",line.color = "gray",line.size=0.4,
         palette = "jco") + stat_compare_means(method = "t.test",paired=TRUE)
# Prueba de Wilcoxon independiente y pareado
# Independiente:
#Alternativa a prueba de t no parametrica
#Se basta en el orden de la observaciones(rankings).Grupos pueden o no tener el mismo numero de datos
#Tambien conocida como Mann-Whitney
#Se combinan las muestras a una sola rankeada
# Hipotesis
# H0: Las dos poblaciones no son diferentes.Probabilidad de que alguien de una poblacion extraido al azar supere a un miembro
# B) Supuestos
#No hay empates en los promedios de Walsh en la prueba pareada
# Distribuciones deben tener la misma forma.
women_weight <- c(38.9,61.2,73.3,21.8,63.4)
men_weight <- c(67.8,60,63.4,75,89.4)
my_data_2 <- data.frame(group = rep(c("Woman","Man"),each=5),weight=c(women_weight,men_weight))
# Estadisticos de resumen
group_by(my_data_2,group) %>% summarise(count = n(),median = median(weight,na.rm=TRUE),
                                        IQR=IQR(weight,na.rm = TRUE))
# Visualimos datos
ggboxplot(my_data_2,x="group",y="weight",
          color="group",palette=c("#00AFBB","#E7B800"),
          ylab="Weight",xlab="Groups")
ggplot(aes(x=weight,fill=factor(group)),data = my_data_2) +
  geom_histogram(position = "dodge",binwidth=15)
# Mecanica de la prueba --- Tendre que rechazar hipotesis nula menor de 0.5 y acepta hipotesis alterna
res <- wilcox.test(women_weight,men_weight)
# Alternativa a una cola
res
wilcox.test(weight~group,data=my_data_2,exact=FALSE,alternative="less")
#Supuestos
ad.test(my_data_2$weight)
# --Pareados
# Las diferencias entre las muestras pareados tener una distribucion simetrica alrededor de la mediana
# Peso antes de tratamiento
antes <- c(200.1,190.9,192.7,213,241.4)
despues <- c(392.9,393.2,342.1,393,434)
my_data_3 <- data.frame(group = rep(c("antes","despues"),each=5), weight=c(antes,despues))
# Estadisticas de resumen
group_by(my_data_3,group) %>% summarise(count = n(),
                                    median=median(weight,na.rm=TRUE),
                                    IQR=IQR(weight,na.rm = TRUE))
# Visualizacion
ggpaired(my_data_3,x="group",y="weight",color="group",palette=c("#00AFBB","#E7B800"),
         ylab="Weight",xlab="Groups",line.color = "gray") + stat_compare_means(paired=TRUE)

# Promedio de Walsh
ggplot(aes(x=weight,fill=factor(group)),data=my_data_3) + geom_histogram(position="dodge",binwidth = 30)
# Mecanica de la prueba
res <- wilcox.test(antes,despues,paired=TRUE)
###### ANOVA
# Descripcion de la prueba
# Analisis de varianza
# Extension de la prueba de t para comparar medias cuando hay mas de  2 grupos.
# Se agrupan datos a traves de un factor
# Hipotesis: Si el valor esperado para cada nivel de factor es el mismo
# H0:control = grupo1 = grupo2
# Ha:al menos uno es distinto
# B:Supuestos
# Las observacions se obtiene completamente al azar y de forma indendiente
# Homocedasticidad de varianza
# Normalidad de residuos
# *Simetria de variables
# Algoritmo basico:
# Determinar varianza intramuestra(residual)
# Determinar varianza entre medias
# Produccion de estadistico de prueba
# Spoiler alert: La ANOVA es un modelo de efecto fijos en variables mudas
# y =  mx + b (Podemos interpretar los ecoficioente de la nova hacia un modelo de regresion lineal)
## Creamos datos
my_data_4 <- PlantGrowth
?PlantGrowth
levels(my_data_4$group)
# Estadistico de resumen -- na.rm parametro para sacar los datos perdidos
group_by(my_data_4,group) %>% summarise(count=n(),
                                     mean=mean(weight,na.rm=TRUE),
                                     sd=sd(weight,na.rm = TRUE))
g1<-ggboxplot(my_data_4,x="group",y="weight",
          color="group",palette = c("#00AFBB","#E7B800","#FC4E07"),
          order = c("ctrl","trt1","trt2"),
          ylab="Weight",xlab="Treatment")
g2<-ggline(my_data_4,x="group",y="weight",
       add=c("mean_se","jitter"),
       order = c("ctrl","trt1","trt2"),
       ylab="Weight",xlab="Treatment")
# Indicando a partir de ahora la visualiazacion va tener 1 fila,3 columna
par(mfrow=c(1,3))
hist((my_data_4%>%filter(group=="ctrl"))$weight)
hist((my_data_4%>%filter(group=="trt1"))$weight)
hist
par(mfrow=c(1,1))
ggarrange(g1,g2)
# Mecanica de la prueba (Correr una Anove)
res.aov <- aov(weight ~ group,data=my_data_4)
# Interpretacion de la prueba
summary(res.aov)
# Validacion de supuestos
hist(res.aov$residuals)
ad.test(res.aov$residuals)
qqPlot(res.aov$residuals,id=FALSE)
leveneTest(weight~group,my_data_4,center=mean)
par(mfrow=c(2,2))
plot(res.aov)
par(mfrow=c(1,1))
# Tukey (ANOVA)
#Creacion de intervalos multiples de confianza
#El algoritmo construye intervalos multiples de confianza para diferencia de medias de "p" poblaciones.
#Define un estadistico llamado de rango estudiantizado
#Se toma en cuenta el numero de compraciones y grados de libertad
TukeyHSD(res.aov)
plot(TukeyHSD((res.aov)))
##### kRUSKAL - WALLIS(no parametrico -- primo de Anova)
# Descripcion de la prueba
# Alternativa no parametrica cuando hay que comparar mas de dos grupos
# 1 variable nominal y una variable rankeada
# No se cumple el supuesto de la simetria/normalidad de la ANOVA
# Si las distribuciones son iguales y simetricas,se compara la mediana
# Confusiooon en la literatura
##No se usa para comparar datos a lo largo del tiempo ni con datos con autocorrelacion espacial
# B Supuestos
#Homocedasticidad
# Todos los grupos deben tener la misma distribucion
# Se construye un estadistico H (mann-whitney construye u)
# Hipotesis
## H0 = Las medias de rango son distintas
# Estadisticos de resumen
group_by(my_data_4,group) %>% summarise(
  count = n(),
  mean = mean(weight,na.rm=TRUE),
  sd = sd(weight,na.rm=TRUE),
  median = median(weight,na.rm=TRUE),
  IQR= IQR(weight,na.rm = TRUE)
)
# Mecanica de la prueba
kruskal.test(weight~group,data=my_data_4)
#Validacion de supuestos -- indica que las varianza son iguales
leveneTest(weight~group,my_data_4,center=median)
?dunn.test
dunn.test(x=my_data_4$weight,g=my_data_4$group,kw=TRUE,alpha=0.05)
##### DATOS CATEGORICAS Y PRUEBAS DIAGNOSTICAS DESDE R
### PAQUETES
pacman::p_load(pROC,OptimalCutpoints,caret,epiR,gglot,dplyr,vcd,tidyverse,vcd,ggthemes,ggpubr,ggsci,blandr,irr)
### Prueba de McNemar
### Coeficiente Kappa
### Matrices de confusion y prueba diagnosticas
### Curvas ROC
### Analisis de Bland-Altman Analysis


# https://www.youtube.com/watch?v=MTgXuyXrHy8&list=PLjjEKe1DTAUbh-u4VdjXmdQ6kMHVokmh_&index=6 5:36

