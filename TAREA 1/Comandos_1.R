####################################
# definir el espacio de trabajo
getwd()             # donde estamos ahora
setwd("~/Documents/Lavoro in corso/Análisis de datos y estadística inferencial - IMCA/Lezioni 2024/Clase_1") # donde vamos
####################################
# R se utiliza como una calculadora de bolsillo
3 + 5                                # sumas
8 - 5                                # diferéncias
7 * 9                                # productios
25 / 5                               # divisiones
2^3                                  # poténcias
sqrt(16)                             # raízes cuadradas
16^(1/4)                             # y otras
####################################
# empleo de paréntesis
4+7 -3
4*7-3
-3 + 4 * 7
-3+4
-3 + 4/2
-(3+4)
-3 + (4/2)
-(3 + 4)/2
####################################
# assinación a un objeto y operaciones
a = 7
a
a <- 7; a
a + 5
b <- 13
c <- a * b; c
e <- c / b + 5 ; e
e <- c / (b + 5) ; e

####################################
# junción de caracteres
nombre   <- "Pedro"
apellido <- "Ramirez"
completo <- paste(nombre, apellido); completo  # con espacio
completo <- paste(nombre, apellido, sep=""); completo # sin espacio
completo <- paste(nombre, apellido, sep=","); completo # con coma

nombre + 5 
####################################
# variables lógicas
s <- TRUE; s
s <- FALSE; s

s <- a+b == 5; s
s <- a+b != 5; s

s <- !(a+b==5); s; !s
####################################
# vectores y operaciones
v <- c(1,2,3); v # come se construye un vector
c(1,2,3) + c(4,5,6)
c(1,2,3) * c(4,5,6)
w <- c(4,5,6)
v + w
z = c(-1.45, sqrt(7), 2^5, sin(1.5))
z
-z
w + z
za <- c(1,2,3,4,5,6); za
za <- c(v,w); za
w + za
v + c("Sergio","Hans","nombre")

v * w
v/w
w ^v

v %*% w   # producto escalar
sum(v*w)  #  4+7-3

length(v)
if(length(v) == length(w)) {v%*%w}
if(length(v) == length(z)) {v%*%z}


v[2]
w[3]
z[c(2,4)]
z[c(2:4)]

vc <- c("uno","dos","tres"); vc        # vector de caracteres
str(vc); typeof(vc); length(vc)
vn <- numeric(length=5); w             # definiciones sin valores
wc <- character(length=4); wc
r  <- rep(1,5); r                      # vector con valores repetidos
f  <- rep(NA,6); f                     # vector con valores faltantes
s  <- seq(1:6); s                      # valore en secuencia
s  <- seq(from=5,to=25,by=5); s
####################################
# matrizes 
m <- matrix(c(3,4,2,1,5,7),nrow = 2,ncol=3); m
m <- matrix(c(3,4,2,1,5,7),nrow = 2,ncol=3,byrow = FALSE); m
n <- matrix(c(3,4,2,1,5,7),nrow = 2,ncol=3,byrow = TRUE); n
dim(m); dim(n)

d <- c(1,2,3,4,5,6,7,8);
dm <- matrix(d,nrow=2,ncol=4); dm
t(dm) # trasposición
str(dm); str(t(dm))
m + n 
m*n
m%*%n # producto filas por columnas
dim(m); dim(n)
m
t(m)%*%n
m%*%t(n)

m[1,2]; m[2,c(2:3)]
m[2,]; m[,2]
m[,c(1,3)]
###################################
# listas
L         <- list(); L
L$a       <- a; L
L$b       <- b; L
L$suma    <- 2*a+3*b; L
L$martriz <- dm; L
str(L)
a         <- 29; L
L$a       <- 29; L
L$b; L[[2]]
###################################
# ¿que tal los objetos?
a; v; m; L
str(a); typeof(a); length(a); dim(a); summary(a) 
str(v); typeof(v); length(v); dim(v); summary(v) 
str(m); typeof(m); length(m); dim(m); summary(m) 
str(L); typeof(L); length(L); dim(L); summary(L) 
####################################