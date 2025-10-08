###########################################################################
# Parte 1 -----------------------------------------------------------------
###########################################################################
#dbh esel diametro de altura de pecho
dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3,
         9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)
#tree son los arboles de pino pero con una secuencia de numeros desde el 1 hasta el 30 (1,2,3,4,5...)
tree <- seq(1,30)
#En las parcelas estoy indicando cuantas columnas y filas quiero con la funcion "gl"
parcelas <- gl(3,10)
#Con "Trees" estoy creando un data.frame con las 3 variables anteriores
Trees <- data.frame(tree,dbh,parcelas)


url <- ("https://repodatos.atdt.gob.mx/api_update/senasica/actividades_inspeccion_movilizacion/29_actividades-inspeccion-movilizacion.csv")
inspeccion <- read.csv(url)
head(inspeccion)  
prof_url_2 <- paste0("https://repodatos.atdt.gob.mx/api_update/senasica/",
                     "actividades_inspeccion_movilizacion/",
                     "29_actividades-inspeccion-movilizacion.csv")



senasica <- read.csv(prof_url_2)
head(senasica)

library(repmis) 
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

head(conjunto) # muestra las primeras seis filas de la BD


library(readr)
file <- paste0("https://raw.githubusercontent.com/mgtagle/",
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")
inventario <- read.csv(file)
head(inventario)

###########################################################################
# Parte 2 -----------------------------------------------------------------
###########################################################################

mean(Trees$dbh) # El signo de $ informa que necesitamos la columna dbh

sd(Trees$dbh)

# Indica la sumatoria de los individuos en el objeto tree con un dbh < a 10
sum(Trees$dbh < 10)

which(Trees$dbh < 10)

trees.13 <- Trees[!(trees$parcela=="2"),]
trees.13

trees.1 <- subset(trees, dbh <= 10)
head(trees.1)

mean(Trees$dbh)



###########################################################################
# Parte 1 -----------------------------------------------------------------
###########################################################################

mamiferos <- read.csv("https://www.openintro.org/data/csv/mammals.csv")
hist(mamiferos$total_sleep)

hist(mamiferos$total_sleep, # Datos
     xlim = c(0,20), ylim = c(0,14), 
     main = "Total de horas sueño de las 39 especies", 
     xlab = "Horas sueño", 
     ylab = "Frecuencia", 
     las = 1, 
     col = "navajowhite") 


data("chickwts")
head(chickwts[c(1:2,42:43, 62:64), ])


feeds <- table(chickwts$feed)
feeds
barplot(feeds)

barplot(feeds[order(feeds, decreasing = TRUE)])

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

###########################################################################
#Laboratorio 1: Empezar co R y Rstudio-------------------------------------
###########################################################################

#Se puede usar como calculadora
#Gastos Totales:
300 + 240 + 1527 + 400 + 1500 + 1833



# a un objeto se le puede asignar un valor:
Celular <- 300
Transporte <- 240
Comestibles <- 1527
Gimnasio <- 400
Alquiler <- 1500
Otros <- 1833



#Se pueden utilizar funciones y la mayoria produce algún tipo de salida
# Valor absoluto (absolute value)
abs(10)
abs(-4)
# Raíz cuadrada (square root)
sqrt(9)
# Logáritmo natural (natural logarithm)
log(2)


#Se pueden poner comentarios usando el simbolo "#"
4 + 5 # también se puede colocar un comentario despues del codigo


#hay que ser cuidadosos con las mayúsculas, ya que significan diferentes cosas
# Detectar mayúsculas y minúsculas
celular <- -300
Celular <- 300
CELULAR <- 8000
celular + Celular

CELULAR - Celular



#Se puede obtener ayudar con la funcion "help" ó con un signo de interrogación antes de la palabra
help(abs)
?abs
#Hay algunas veces que no se conoce la funcion, sin embargo puedes buscar algunas palabras clave
help.search("absolute")
??absolute



#Se va a crear un vector llamado "gastos"
gastos <- c(Celular, Transporte, Comestibles, Gimnasio, Alquiler, Otros)

#Y ahora se va a graficar usando la función "barplot"
barplot(gastos)






