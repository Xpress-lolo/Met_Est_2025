#===============================================================================
#Correlación de Pearson
#Datos del geyser Old Faithful
#24/09/2025
#===============================================================================

data("faithful")

plot(faithful$waiting, faithful$eruptions,
     xlab= "Tiempo de espera (min)",
     ylab= "Duracion de la erupción (min)",
     col="purple",
     pch=20)
################################################################################
#Correlacionar las dos variables ###############################################

shapiro.test(faithful$eruptions)
shapiro.test(faithful$waiting)
################################################################################
#Pearson solo se utiliza cuando tenemos datos normales##########################
cor.test(faithful$waiting, faithful$eruptions, 
         method= "pearson")




################################################################################
#Spearman se utiliza como contraparte de datos no normales######################
#(Ordena los datos de manera ascendente)

cor.test(faithful$waiting, faithful$eruptions, 
         method= "spearman")
#===============================================================================
#===============================================================================
#25/09/2025

resp <- data.frame(
  Tiempo=c(12,15,17,18,20,21,22,26),
  Edad=c(13,25,20,35,45,30,60,95)
)

resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method="first")
resp$Rango_Edad <- rank(resp$Edad, ties.method="first")

plot(resp$Tiempo, resp$Edad,
     xlab="Tiempo",
     ylab="Edad",
     col="indianred")
plot(resp$Rango_Tiempo, resp$Rango_Edad,
     xlab="Tiempo",
     ylab="Edad",
     col="peru")


resp$dif <- resp$Rango_Tiempo - resp$Rango_Edad
resp$dif2 <- resp$dif^2
sum(resp$dif2)

cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method="spearman")
cor.test(resp$Tiempo,resp$Edad,method="spearman")

#===============================================================================
#===============================================================================
#===============================================================================
#Tau de Kendall
tau <- data.frame(
  A=c(1,2,3,4,5,6),
  B=c(3,1,4,2,6,5))
cor.test(tau$A, tau$B, method="kendall")

plot(tau$A,tau$B,
     col="green")

#===============================================================================
#===============================================================================
#===============================================================================
#Punto Biserial

set.seed(123)
n <- (20)
Horas_estudio <- sample(1:10,n,replace=T)
Resultado <- sapply(Horas_estudio, function(horas){ifelse(runif(1)<(horas/10),"Aprobado","Reprobado")})

estudio <- data.frame(
  Estudiante=1:n,
  Horas_estudio,
  Resultado
)

estudio$Resultado_bin <- ifelse(estudio$Resultado=="Aprobado",1,0)
head(estudio)

cor.test(estudio$Horas_estudio, estudio$Resultado_bin,method="pearson")
mean_aprobados <- mean(estudio$Horas_estudio[estudio$Resultado=="Aprobado"])
mean_aprobados

mean_reprobados <- mean(estudio$Horas_estudio[estudio$Resultado=="Reprobado"])
mean_reprobados








































































