#Pruebas de t
##Caso de muestras independientes
#27/08/2025

#Importar datos de Indice de Calidad
Calidad.de.planta <- read.csv("C:/Repositorio Oliver/Met_Est_2025/Semana 4 clase 1/Calidad de planta.csv", header=T)

Calidad.de.planta$Tratamiento <- as.factor(Calidad.de.planta$Tratamiento)

boxplot(Calidad.de.planta$IE ~ Calidad.de.planta$Tratamiento,
        col="peru",
        xlab="tratamientos",
        ylab="Indice calidad",
        ylim=c(0.4,1.2),
        main="Vivero Iturbide")

#Estadística descriptiva

planta <- Calidad.de.planta$planta
IE <- Calidad.de.planta$IE
Tratamiento <- Calidad.de.planta$Tratamiento

### tapply sirve para obtener un valor cuando contamos con varios grupos
tapply(Calidad.de.planta$IE,Calidad.de.planta$Tratamiento,mean) #Se utilizó (Calidad.de.planta$IE) para saber como se saca, sin embargo,
                                                                #abajo lo simplifique creando otros apartados que valen lo mismo
tapply(IE,Tratamiento,var)

##Observamos que la varianza del grupo fert es 3 veces mas grande que el grupo control
#Se instaló ggplot2

library("ggplot2")

ggplot(Calidad.de.planta, aes(x=IE,color=Tratamiento))+
  geom_density()

