#########################################################################
# clasificación jerarquica con hclust
# Pero se necesita de calcular la matriz  de distancias previamente.
dat = read.csv("Linnerud.csv",header=TRUE,row.names=1);head(dat)
n <- dim(dat)[1]
lin.dis   <- dist(dat); round(lin.dis,3)         # matriz de distancias (Euclidianas)       
lin.clust <- hclust(lin.dis,method="ward.D2")    # jerarquía ("Ward")
plot(lin.clust)                                  # gráfico dendrograma
plot(lin.clust,hang=-1)                          # gráfico mejor
lin.clust                                        
str(lin.clust)
# hclust brinda muchas salidas: se les vamos a salir todas
summary(lin.clust)
lin.clust$merge        # las parejas de nodos que se juntan
lin.clust$height       # el nivel de junctión
lin.clust$order        # el orden de las hojas
lin.clust$labels       # nombres
lin.clust$method       # método  
lin.clust$call         # fila de llamada
lin.clust$dist.method  # la distancia
part <- cutree(lin.clust, k = 1:(n-1)); part  # así se hacen todas las particiones posibles 
############################################################################################
# elección de la partición: inicialización
maxcl    <- n-1
ngr      <- c((maxcl):1)
wss      <- rep(0,maxcl)
wssd     <- wss
wssd2    <- wss
##########################################################################################
# estadísticas para el índice de la jerarquía
indx     <- lin.clust$height                       
Di       <- c(0,indx[2:maxcl] - indx[1:(maxcl-1)])    # diferencias índice de Ward
Di2      <- c(0,Di[2:maxcl] - Di[1:(maxcl-1)])        # diferencias segundas
########################################################################
# función que calcula la suma de cuadrados centrados

css <- function(data) {            # Centered sum of squares of a data.frame
       sum(scale(data, center = TRUE, scale = FALSE)^2)
}
########################################################################
# función que particiona los datos según los grupos eligidos y calcula
# la wss correspondiente
wrap <- function(i, hc, x) {          #  i = # of clusters, hc = hclust, x = data
        cl  <- cutree(hc, i)          # gets the partition in i groups 
        spl <- split(x, cl)           # splits the data in classes
        wss <- sum(sapply(spl, css))  # computes the wss
        wss
}
########################################################################
# se aplica wrap a cada partición
wss      <- rep(0,maxcl)
length(wss)
for (i in 1:maxcl) {
  wss[i] <- wrap(i,lin.clust,dat)             # suma de cuadratos dentro
}
# se calculan estadísticas correspondientes
Dw       <- -c(wss[2:maxcl] - wss[1:(maxcl-1)],0); Dw     # las dos diferencias
Dw2      <- -c(Dw[2:maxcl] - Dw[1:(maxcl-1)],0); Dw2

tss      <- wss[1]                            # suma de cuadratos total
bss      <- tss - wss                         # suma de cuadratos entre
# índice de Calinski-Harabász
df1      <- c(1:(maxcl))                      # número de grupos
df2      <- (n-c(1:maxcl))                    # número de observaciones - grupos
CH       <- (bss/df1)/(wss/df2)               # Calinski-Harabász
# se c onstruye la tabla de resultados
index    <- cbind(c((maxcl):1),lin.clust$merge,indx,Di,Di2,rev(wss),rev(Dw),rev(Dw2),CH)
rownames(index)   <- c(1:maxcl)
colnames(index)   <- c("ngr","g1","g2","index","Dindex","D2index","wss","Dwss","Dwss2","CH")
index
###### gráfico del índice
plot(c(1:maxcl),indx, type="l", xlab="Number of Clusters", ylim = c(min(indx,Di,Di2),max(indx,Di,Di2)),xaxt="n",
     ylab="Ward's criterion index", main = "Ward index variation")
axis(1,at=c(1:maxcl),labels=ngr)
lines(c(1:maxcl),Di,col="blue")
lines(c(1:maxcl),Di2,col="red")
legend("topleft",legend=c("Ward's Index","Differences","Second differences"),col=c("black","blue","red"),lty=1)
###### gráfico de las inercias
plot(1:maxcl, wss, type="l", xlab="Number of Clusters", ylim = c(min(wss,Dw,Dw2),max(wss,Dw,Dw2)),xaxt="n",
     ylab="Ward's criterion index", main = "Ward criterion, inertia variation")
axis(1,at=c(1:maxcl),labels=1:maxcl)
lines(1:maxcl, Dw,col="blue")
lines(1:maxcl, Dw2,col="red")
legend("topright",legend=c("Ward's Inertia","Differences","Second differences"),col=c("black","blue","red"),lty=1)
###### gráfico de Calinski-Harabász
plot(1:maxcl, CH, type="l", xlab="Number of Clusters", xaxt="n",
     ylab="CH index", main = "Ward criterion, Calinski-Harabász index")
axis(1,at=c(1:maxcl),labels=c(1:maxcl))
###########################################################################################
# estudio de una partición
ncl    <- 4                  # por ejemplo en 4 clases, pero cuidado, pues ncl es empleado
                             # para eligir la cuarta partición en part, ya que se han 
                             # hechas todas con cutree. Si no, hay que encuentrar cual se
                             # corresponde en part a ncl clases.
parti  <- part[,ncl]; parti  # clase de cada unidad
# para saber la lista de unidades por grupo en una partición y el promedio de las variables
apply(dat,2,mean)
clases = list()
dat_clases = split(dat,parti)
for (k in 1:ncl) {   # se buscan los nombres de las unidades
  clases$ind[[k]] <- rownames(part)[which(part[,ncl]==k)] # identifica ls puntos,
  clases$mean[[k]]<- apply(dat_clases[[k]],2,mean)        # cálcula los promédios
}
clases                                                    # lista de salida
###### dendrograma con clases
plot(lin.clust,hang=-1)
rect.hclust(lin.clust,k=ncl)
abline(h=(mean(indx[maxcl-ncl+c(1,2)])),col="red")
#########################################################################
# clasificación jerárquica con similitud
# se trabaja con Ellenberg
ell <- read.csv("Ellenberg.csv",header=TRUE,row.names=1); head(ell)
n = dim(ell)[1]
p = dim(ell)[2]
ell.c.sim <- matrix(0,nrow=p,p)
# se construye la matriz de similitud entre columnas según Sorensen 
for (i in 1:p) {
  for (j in 1:i) {
    if (i ==j)  {
      ell.c.sim[i,i] <- 1
    } else {
      t  <- table(ell[,i],ell[,j])  # tabla de contingencia
      a  <- t[1,1]
      b  <- t[1,2]
      c  <- t[2,1]
      d  <- t[2,2]
      so <- 2*a / (2*a + b + c)    # sorensen
      ell.c.sim[i,j] <- so         # valores en la primera mitad
      ell.c.sim[j,i] <- so         # también otra mitad
    } 
  }     
}
ell.c.sim
# se transforma en disimilitud y se hace la jerarquía
eld       <- as.dist(1-(ell.c.sim))
ell.c.cpl <- hclust(eld,method="complete") # distancia máxima
# gráfico del dendrograma
plot(ell.c.cpl,hang=-0.1)

ell.s.cpl <- hclust(eld,method="single")   # distancia mínima
plot(ell.s.cpl,hang=-0.1)
########################################################################
# trabajamos con la máxima distancia
maxcl    <- p-1
wss      <- rep(0,maxcl)
wssd     <- wss
wssd2    <- wss
wss      <- ell.c.cpl$height
for (i in 1:maxcl) {
  if (i>1) {
    wssd[i]  <- wss[i]-wss[i-1]
  }
  if (i>2) wssd2[i] <- wssd[i]-wssd[i-1]
}
index             <- cbind(c(maxcl:1),wss,wssd,wssd2)
rownames(index)   <- c(1:maxcl)
colnames(index)   <- c("ngr","SSW","D","D2")
index                                         # tabla de resultados
plot(ell.c.cpl,hang=-0.1)                     # gráfico con clases
rect.hclust(ell.c.cpl,k=5)                    # se dibujan rectangulos para clases
########################################################################
# cuidado en seguida, porque, si se quiere hacer el mismo estudio que para Linnerud,
# hay que transponer ell, (comando elt <- t(ell)) y trabayar con elt, porque los comandos 
# siguientes funcionan para filas y no para columnas. Al contrario, si se van clasificando
# las filas hay que hacer atención en la construcción de la matriz de similitud, pero luego
# todo debería funcionar como en Linnerud. Atención también a corregir en todos casos las 
# dimensiones, que arriba se refieren a Linnerud.
########################################################################################