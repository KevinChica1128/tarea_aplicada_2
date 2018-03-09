#Tarea 2 aplicada
library(readxl)
empleados1 <- read_excel("empleados1.xlsx")
empleados1M<-empleados1[c(4,10,12,26,28,33,43,45,56,71,79,95),]
empleados1H<-empleados1[-c(4,10,12,26,28,33,43,45,56,71,79,95),] #Me deja solo los hombres para hacer el muestreo
sample(1:87,12) #Me genera 12 números aleatorios, correspondientes a los hombres que vamos a muestrear
#Los generados fueron: (10,29,9,13,47,45,2,79,69,15,41,3)
empleados1H<-empleados1H[c(10,29,9,13,47,45,2,79,69,15,41,3),] #Muestra de 12 hombres obtenida
empleados1<-rbind(empleados1H,empleados1M) #Me pone en una misma base de datos los hombres y la mujeres, esta será nuestra base de datos final.
#Descriptivas peso y estatura:
rbind(Peso=summary(empleados1$Peso),Altura=summary(empleados1$Altura),Edad=summary(empleados1$Edad))

#punto 1 y 2:
modelo<-lm(empleados1$Peso~empleados1$Altura)
summary(modelo)$sigma^2

#Gráfica de regresión:
x11()
plot(empleados1$Altura, empleados1$Peso, xlab = "Estatura", ylab = "Peso",col="Black",pch=16)
abline(modelo,col="Red")

x11()
plot(empleados1H$Altura,empleados1H$Peso,ylim=c(50,105),xlim = c(155,200),xlab = "Estatura", ylab = "Peso",col="Blue",pch=16)
abline(modelo,col="Red")
points(empleados1M$Altura,empleados1M$Peso,pch=16,col="Green")
legend("topright",legend=c("Hombre","Mujer"),col=c("Blue","Green"),pch=c(16,16))
#punto 3:
sigma2<-summary(modelo)$sigma^2
x<-matrix(c(rep(1,24),empleados1$Altura),ncol = 2,nrow = 24)
Cjj<-solve(t(x)%*%x)
B0=-99.0330
B1=0.9778
alfa=0.05
#Intervalo para B0:
intervalo<-c(B0+qt(alfa/2,24-2)*sqrt(Cjj[1,1]*sigma2),B0-qt(alfa/2,24-2)*sqrt(Cjj[1,1]*sigma2))
#Intervalo para B1:
intervalo1<-c(B1+qt(alfa/2,24-2)*sqrt(Cjj[2,2]*sigma2),B1-qt(alfa/2,24-2)*sqrt(Cjj[2,2]*sigma2))
#Con la función de R:
confint(modelo,level = 0.95)
#punto 4 y 5:
#Vamos a representar los hombres con 1 y las mujeres con 0
library("car")
sexocodificado<- recode(empleados1$Sexo, '"Hombre"=1; "Mujer"=0; ;', as.factor.result=TRUE)
empleados1<-cbind(empleados1,sexocodificado)

#Modelo teniendo en cuenta la interacción entre el sexo y la altura:
x3<-c(200,174,181,173,172,179,168,167,165,170,179,194,0,0,0,0,0,0,0,0,0,0,0,0)
modeloint<-lm(empleados1$Peso~empleados1$Altura+empleados1$sexocodificado+x3)
summary(modeloint)

#Gráfica con ambas rectas ajustadas
x11()
plot(empleados1H$Altura,empleados1H$Peso,ylim=c(50,105),xlim = c(155,200),xlab = "Estatura", ylab = "Peso",col="Blue",pch=16)
abline(modeloHombres,col="Blue")
abline(modeloMujeres,col="Green")
points(empleados1M$Altura,empleados1M$Peso,pch=16,col="Green")
legend("topright",legend=c("Hombre","Mujer"),col=c("Blue","Green"),pch=c(16,16))
#punto 6:
#Modelo con la variable edad:
modeloSE<-lm(empleados1$Peso~empleados1$Altura+empleados1$sexocodificado+x3+empleados1$Edad)
summary(modeloSE)$sigma^2


#--------------------------------------------------------------#

modeloMujeres<-lm(empleados1M$Peso~empleados1M$Altura)
modeloHombres<-lm(empleados1H$Peso~empleados1H$Altura)
summary(modeloMujeres)
summary(modeloHombres)
