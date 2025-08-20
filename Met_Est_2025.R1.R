#Laboratorio Semana 3
# 20/08/2025
#Oliver Berlanga sanchez


#Importar datos -----------------------------------------------------------

Temp <- read.csv("temperatura.csv", header= T)
Temp <- read.csv("Datos/Medias_temp.csv", header=T)


#Ingresar Datos de Manera Manual

edad <- c(18,19,18,18,25,19,18,18,18,17,19,19,18,17,19,
          18,19,19)
alumno <- seq(1,18,1)

info <- data.frame(alumno,edad)

info$Altura <- c(174,174,170,160,158,155,188,
                 170,175,170,172,170,174,180,
                 158,161,188,164)
# Graficar Datos ----------------------------------------------------------

boxplot(info$Altura,
        #col sirve para ponerle color a la grafica
        col="snow4",
        #main sirve para poner un titulo
        main="Clase 3 Semestre")

colores=c("peru", "olivedrab","firebrick4")
boxplot(datos_meses,
        col = colores,
        main="Temperatura de los meses")
# Estadisticas Descriptivas -----------------------------------------------



# Estadistica Inferencial -------------------------------------------------


