costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95, 73.59, 77.92,
            77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99, 81.94, 80.41, 77.7)

media <- mean(costal)
varianza <- var(costal)
t.test(costal, mu=80)

#H0 = es igual a 80
#H1 = no es igual a 80

#producción de semillas
sem <- read.csv("C:/Repositorio Oliver/Met_Est_2025/10-09-2025/mainproduccion.csv", header= T)
sem$Tiempo <- as.factor(sem$Tiempo)

tapply(sem$Kgsem, sem$Tiempo, mean)

boxplot(sem$Kgsem ~ sem$Tiempo,
        col="peru",
        xlab="año",
        ylab="semilla (kg)")

t2012 <- subset(sem,sem$Tiempo == "T2012")
t2013 <- subset(sem,sem$Tiempo != "T2012")

t.test(t2012$Kgsem, t2013$Kgsem, paired=T,alternative="less")
