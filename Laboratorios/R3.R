temperatura <- read.csv("C:/Repositorio Oliver/Met_Est_2025/temperatura.csv")
View(temperatura)

head(temperatura) #primeras 6 filas de datos
dim(temperatura) #numero de filas y columnas
names(temperatura) #nombres de las columnas
str(temperatura) #estructura del objeto

#Resumen estadistico
summary(temperatura)

names(temperatura) <- c("Ang", "Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", 
                        "Agostino Langostino", "Sep", "Oct", "Nov", "Dic")


names(temperatura)

#Crear columna Media_anual con temperatura media anual
temperatura$Ene
temperatura$Media_anual <- rowMeans(temperatura[,2:13])
head(temperatura)

#Crear objeto con medias mensuales de temperatura
medias_mensuales <- colMeans(temperatura[,2:13])
medias_mensuales
help(boxplot)

#boxplot para un solo mes
boxplot(temperatura$Ene,
        main="temperatura de enero",
        ylab="°C",
        col="lightblue")

datos_meses <- temperatura[,2:13]
boxplot(datos_meses,
        main="Temperatura de enero",
        ylab="°C",
        col="yellow",
        names=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", 
                "Agostino Langostino", "Sep", "Oct", "Nov", "Dic"))
