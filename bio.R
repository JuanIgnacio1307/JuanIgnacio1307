library(base);
library(graphics);
###### Crear funcion Michaelis-Menten
V <- function(S,Vmax,Km){ Vmax*S/(Km+S) }
####### Datos #########
S1 <- c(2.5e-06, 3.33e-06, 4.0e-06, 5.0e-06, 1.0e-05, 2.0e-05, 4.0e-05, 1.0e-04, 
2.0e-03, 1.0e-02)  # M (molar)
v1 <- c(24, 30, 34, 40, 60, 80, 96, 109, 119, 120) # uM/min
S2 <- c(10e-06, 40e-06, 80e-06, 200e-06, 80e-05, 120e-05, 200e-05, 40e-04, 8e-03, 
1.0e-02)  # M (molar)
v2 <- c(22, 30, 50, 70, 103, 110,115 , 118, 119, 120) # uM/min
datos1 <- data.frame(S1, v1) 
datos2 <- data.frame(S2, v2)
########## Ajuste de Curva ########
Mfit1 <- nls(v1~V(S1,Vmax1,Km1), datos1, start=list(Vmax1=100, Km1=0.1))
Mfit2 <- nls(v2~V(S2,Vmax2,Km2), datos2, start=list(Vmax2=120, Km2=0.03))
####### Resultados del Ajuste Curva 1 #####
Vmax_fit1 <- coef(Mfit1)[1]   # uM/min
Km_fit1 <- coef(Mfit1)[2]   # M
print(Vmax_fit1)
print(Km_fit1)
####### Resultados del Ajuste Curva 2 #####
Vmax_fit2 <- coef(Mfit2)[1]   # uM/min
Km_fit2 <- coef(Mfit2)[2]   # M
print(Vmax_fit2)
print(Km_fit2)
########Graficar Datos y Ajuste de Curva ####
x11( height= 7, width=14)
par(mar=c(4,5,1,0.2),bg="white")
### Ploteo de datos ####
plot(datos1, type="p", col="red", main="V vs. [S]", xlab="[S](M)", 
ylab="V(uM/min)", cex.lab=1.5, cex=2, pch = 19 )
#####Ploteo de Resultados ####
puntos <- 1000
Sconc <- c(0:puntos) *S1[length(S1)] / puntos
lines(Sconc, V(Sconc, Vmax_fit1, Km_fit1), type="l", col="blue", lwd=4)
lines(S2, v2, type="p", col="orange", lwd=4, cex.lab=1.5, cex=2, pch = 19)
lines(Sconc, V(Sconc, Vmax_fit2, Km_fit2), type="l", col="cyan", lwd=4)
