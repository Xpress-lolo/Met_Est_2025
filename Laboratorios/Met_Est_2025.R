# Gastos mensuales
300+240+1527+400+1500+1833
celular <- 300
celular
transporte <- 240
comestibles <- 1527
gimnasio <- 400
alquiler <- 1500
otros <- 1833
alquiler+celular+comestibles+gimnasio+otros+transporte
total <- alquiler+celular+comestibles+gimnasio+otros+transporte
semestre <- total*5 
anual <- total*10

#Valor absoluto
abs(10)
abs(-4)

#Raiz cuadrada
sqrt(9)

#logaritmo natural
log(2)

2*9
4+5 #se puede poner comentarios en las lineas

#Se identifican mayusculas
celular <- 300
Celular <- -300
CELULAR <- 8000

celular+Celular
CELULAR-celular
help(abs)
help(mean)
?abs
?mean
help.search("absolute")
??absolute

gastos <- c(celular,transporte,comestibles,gimnasio,alquiler,otros)

barplot(gastos)
barplot(sort(gastos))
??sort
gastos_ord <- sort(gastos, decreasing = TRUE)
sort(gastos)
barplot(sort(gastos,decreasing=TRUE))
barplot(gastos_ord)
help("barplot")
barplot(gastos_ord, main="gastos mensuales",names.arg=c("otros", "comestibles", "alquiler", 
                                                        "gimnasio", "celular", "transporte"))
