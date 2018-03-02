#Tarea 2 aplicada
library(readxl)
empleados1 <- read_excel("empleados1.xlsx")
empleadis1M<-empleados1[c(4,10,12,26,28,33,43,45,56,71,79,95),]
empleados1H<-empleados1[-c(4,10,12,26,28,33,43,45,56,71,79,95),] #Me deja solo los hombres para hacer el muestreo
sample(1:87,12) #Me genera 12 números aleatorios, correspondientes a los hombres que vamos a muestrear
#Los generados fueron: (10,29,9,13,47,45,2,79,69,15,41,3)
empleados1H<-empleados1H[c(10,29,9,13,47,45,2,79,69,15,41,3),] #Muestra de 12 hombres obtenida
empleados1<-rbind(empleados1H,empleadis1M) #Me pone en una misma base de datos los hombres y la mujeres, esta será nuestra base de datos final.

#punto 1 y 2:
modelo<-lm(empleados1$Peso~empleados1$Altura)
summary(modelo)

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


#punto 4 y 5:
#Vamos a representar los hombres con 1 y las mujeres con 0
library("car")
sexocodificado<- recode(empleados1$Sexo, '"Hombre"=1; "Mujer"=0; ;', as.factor.result=TRUE)
empleados1<-cbind(empleados1,sexocodificado)
#Modelo con la variable sexo:
modelosex<-lm(empleados1$Altura~empleados1$Peso+empleados1$sexocodificado)
summary(modelosex)

#punto 6:
#Modelo con la variable edad:
modeloSE<-lm(empleados1$Altura~empleados1$Peso+empleados1$Edad+empleados1$sexocodificado)
summary(modeloSE)
