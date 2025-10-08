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

sd(IE,"na.rm"=FALSE)


#Vamos a separar los datos por tratamiento

df_ctlr <- subset(Calidad.de.planta, Tratamiento == "Ctrl")
df_fert <- subset(Calidad.de.planta, Tratamiento != "Ctrl")


#Vamos a revisar la normalidad de los datos

par(mfrow=c(1,2)) #Este es para poder ver al mismo tiempo los dos graficos de abajo, le estoy diciendo que quiero una fila y dos columnas
qqnorm(df_ctlr$IE);qqline(df_ctlr$IE) 
qqnorm(df_fert$IE);qqline(df_fert$IE)

par(mfrow=c(1,1)) #Este es para volver a la normalidad, le estoy diciendo que quiero una fila y una columna


shapiro.test(df_ctlr$IE)
shapiro.test(df_fert$IE)

#vamos a revisar las varianzas         ##    
#################################         ##     
var.test(IE ~ Tratamiento)      #   #####   ## 
var.test(df_ctlr$IE,df_fert$IE) #   #####   ##   Se puede de cualquiera de las dos formas
#################################         ##
                                       ##    

#Aplicar la prueba de t, varianzas iguales
#dos colas=two.sided

t.test(IE~Tratamiento,var.equal=TRUE,
       alternative="two.sided")

cohens_efecto <- function(x,y) {
  n1 <- length(x); n2 <- length(y) 
  s1 <- sd(x);   s2 <- sd(y)
  sp <- sqrt(((n1-1) * s1^2 + (n2-1) * s2^2) / (n1+n2-2))
  (mean(x)-mean(y))/sp } 

d_cal <- cohens_efecto(df_ctlr$IE, df_fert$IE)






































