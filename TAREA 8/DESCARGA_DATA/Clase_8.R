#################################################################################################################
# tentativo de análisis con kmeans
# trabajamos con Linnerud
linnerud <- read.csv("Linnerud.csv",header = TRUE, row.names = 1)
#  inicialización
dat      <- linnerud
n        <- dim(dat)[1]
p        <- dim(dat)[2]
maxcl    <- round(n/2,0)
wss      <- rep(0,maxcl)
wssd     <- wss
wssd2    <- wss
CH       <- wss
wss[1]   <- (n-1)*sum(apply(dat,2,var)); wss
#  varios tentativos con varias clases
for (i in 1:maxcl) {
  wss[i] <- sum(kmeans(dat,centers=i)$withinss);       # colectando la ssw
  if (i>1) {
    wssd[i-1]  <- wss[i-1]-wss[i]
    CH[i] <- ((wss[1] - wss[i])/(i-1))/(wss[i]/(n-i))  # calculando el índice
  }                                                    # de Calinski-Harabász 
  if (i>2) wssd2[i-2] <- wssd[i-2]-wssd[i-1]           # pero también diferencias
}
index             <- cbind(wss,wssd,wssd2,CH)
rownames(index)   <- c(1:maxcl)
colnames(index)   <- c("SSW","D","D2","CH")
index                                                  # tabla 
################################################################################################################
# gráfico de los valores
plot(1:maxcl, index[,1], type="l", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main = "K-means inertia variation")
  lines(1:maxcl, index[,2],col="blue")
  lines(1:maxcl, index[,3],col="red")
  legend("topright",legend=c("Within sum of squares","Differences","Second differences"),col=c("black","blue","red"),lty=1)
dev.copy(pdf,"linnerud_kmeans_wss.pdf")
dev.off()  
  
plot(1:maxcl, index[,4], type="l", xlab="Number of Clusters",
     ylab="Calinski-Harabász index", main = "K-means inertia variation")  
dev.copy(pdf,"linnerud_kmeans_CH.pdf")
dev.off()  
################################################################################################################
# kmeans con cuatro grupos y tres diferentes selecciones iniciales
dat.kcl <- kmeans(dat,centers=4,nstart=3)
dat.kcl
summary(dat.kcl)
dat.kcl$cluster        # atribución de las unidades a las clases
dat.kcl$centers        # promedios de caracteres en cada clase
dat.kcl$totss          # total sum of squares
dat.kcl$tot.withinss   # total within sum of squares
dat.kcl$betweenss      # between sum of squares
dat.kcl$size           # tamaño de las clases
dat.kcl$iter           # número de iteraciones  
dat.kcl$ifault
################################################################################################################
