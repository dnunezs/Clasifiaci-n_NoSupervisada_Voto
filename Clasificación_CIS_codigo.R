# Métodos no supervisados de agregación del voto con datos del CIS 
## kmeans
  
#Cargamos librerias que podemos necesitar
library(readxl)
library(cluster)
#Cargamos la base
cis3242 <- read_excel("CIS3242.xlsx")



#Ordenamos y mostramos los valores que toma cada variable
sort(unique(cis3242$VOTOSIMG))
sort(unique(cis3242$Ideologia))
sort(unique(cis3242$Edad))
sort(unique(cis3242$ProbVoto))

#Recodificamos la variable de voto según cumplen las condiciones
cis3242$votorec <- ifelse(cis3242$VOTOSIMG==1, 1, 
                          ifelse(cis3242$VOTOSIMG==2, 2, 
                                 ifelse(cis3242$VOTOSIMG==21 | cis3242$VOTOSIMG==6 | cis3242$VOTOSIMG ==37 | cis3242$VOTOSIMG==10, 3,
                                        ifelse(cis3242$VOTOSIMG==4, 4, 
                                               ifelse(cis3242$VOTOSIMG==18, 5,99)))))

cis3242 <- subset(cis3242, cis3242$votorec != 99 & cis3242$Ideologia != 98 & cis3242$Ideologia != 99 & cis3242$ProbVoto != 98 & cis3242$ProbVoto != 99   )

summary(cis3242)

cis3242$votorec <- factor(cis3242$votorec, levels=c(1,2,3,4,5), labels=c("PP", "PSOE", "UP", "Cs", "Vox"))
str(cis3242)

#Seleccionamos solo las variables que vamos a usar
cis3 <- cis3242[,2:4]

#Se mete en un bucle para ir ver cual sería el mejor cluster
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(cis3, i)
  resultados[i] <- fit$tot.withinss
}

#Visualizamos los resultados
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Número de clusters",ylab="tot.tot.withinss")

#Cargamos la libreria
library(fpc)

#Usamos la funcion kmeansruns para ver los otros dos modelos
fit_ch  <- kmeansruns(cis3, krange = 1:10, criterion = "ch") 
fit_asw <- kmeansruns(cis3, krange = 1:10, criterion = "asw")

#Vemos los valores de los dos modelos
fit_ch$bestk
fit_asw$bestk

#Vemos en graficos los dos modelos
plot(1:10,fit_ch$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio Calinski-Harabasz")
plot(1:10,fit_asw$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio silueta media")

#Calculamos el cluster con Kmean haciendo cinco grupos
cis2clusters <- kmeans(cis3, 5)

#Lo vemos con edad e ideologia
plot(cis3[c(2,3)], col=cis2clusters$cluster)
#Lo vemos con edad e ideologia
plot(cis3[c(1,2)], col=cis2clusters$cluster)
#Lo vemos con edad e ideologia
plot(cis3[c(1,3)], col=cis2clusters$cluster)

#Nos quedamos solo con ideologia y porbabilidad de votar
cis3 <- cis3242[,2:3]

#Vemos el numero de cluster optimo con tot.tot.withinss
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(cis3, i)
  resultados[i] <- fit$tot.withinss
}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Número de clusters",ylab="tot.tot.withinss")

fit_ch  <- kmeansruns(cis3, krange = 1:10, criterion = "ch") 
fit_asw <- kmeansruns(cis3, krange = 1:10, criterion = "asw")
fit_ch$bestk
fit_asw$bestk
plot(1:10,fit_ch$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio Calinski-Harabasz")
plot(1:10,fit_asw$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio silueta media")

set.seed(567)
cis2clusters <- kmeans(cis3, 5)


plot(cis3[c(1,2)], col=cis2clusters$cluster)

#Vemos en una tabla el cruce de los grupos creado con los grupo de la base original
table(cis2clusters$cluster,cis3242$votorec)

#Cruce cluster por ideologia
table(cis2clusters$cluster,cis3242$Ideologia)

#Acierto
(1122+1597+99+258+18)/nrow(cis3242)*100


cis2clusters <- kmeans(cis3, 3)

# sepalLength y sepalWidth
plot(cis3[c(1,2)], col=cis2clusters$cluster)

#Vemos en una tabla el cruce de los grupos creado con los grupo de la base original
table(cis2clusters$cluster,cis3242$votorec)

#Cruce cluster por ideologia
table(cis2clusters$cluster,cis3242$Ideologia)

#Cruce cluster por probabilidad de voto
table(cis2clusters$cluster,cis3242$ProbVoto)

## CLARA

#Cargamos la libreria
library(factoextra)

#Ejecutamos el metodo CLARA
claracis <- clara(x = cis3, k = 5, metric = "manhattan", stand = TRUE,
                  samples = 50, pamLike = TRUE)
#Vemos el resutlado
claracis

#Realizamos un grafico
fviz_cluster(object = claracis, ellipse.type = "t", geom = "point",
             pointsize = 2) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

#Vemos el resutlado en un tabla
table(claracis$cluster,cis3242$votorec)

#Acierto
(1151+2469+6+858+44)/nrow(cis3242)*100

## Hierarchical cluster

#Calculamos las distancias
set.seed(101)
matriz_distancias <- dist(x = cis3, method = "euclidean")

set.seed(567)
hc_euclidea_completo <- hclust(d = matriz_distancias, method = "complete")
hc_euclidea_single   <- hclust(d = matriz_distancias, method = "single")
hc_euclidea_average  <- hclust(d = matriz_distancias, method = "average")

#Vemos el dendograma para ver cuales son los mejores corte para seleccionar los grupos
par(mfrow = c(3,1))
plot(x = hc_euclidea_completo, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage complete")
plot(x = hc_euclidea_single, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage single")
plot(x = hc_euclidea_average, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage average")


#Lo vemos en un atabla
table(cutree(hc_euclidea_completo, k = 5), cis3242$votorec)
